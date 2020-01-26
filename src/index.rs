//! A lazily populated index for looking up entities by name.

#![allow(unused)]

use clang::{self, Entity, EntityKind};
use std::collections::{hash_map, HashMap};
use std::fmt::{self, Display};
use thiserror::Error;

/// Represents errors which can occur while looking up entities in the index.
#[non_exhaustive]
#[derive(Error, Debug)]
pub(crate) enum LookupError {
    #[error("couldn't find `{0}`")]
    NotFound(Path),
    #[error("{0}")]
    SourceError(clang::SourceError),
}

/// The result of a lookup operation.
type Result<T> = std::result::Result<T, LookupError>;

/// An ID assigned to a `Path` in the index.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct NodeId(u32);

impl NodeId {
    #[inline(always)]
    fn from_usize(n: usize) -> NodeId {
        NodeId(n as u32)
    }
    #[inline(always)]
    fn as_usize(&self) -> usize {
        self.0 as usize
    }
}

/// A C++ unqualified identifier.
///
/// Examples: `std`, `vector`, or `MyClass`.
#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub(crate) struct Ident {
    s: String,
}
impl From<&str> for Ident {
    /// Creates an identifier. Can panic if the identifier is invalid.
    fn from(id: &str) -> Ident {
        assert!(!id.contains("::"), "invalid identifier `{}`", id);
        Ident { s: id.to_string() }
    }
}
impl From<String> for Ident {
    fn from(id: String) -> Ident {
        From::from(id.as_str())
    }
}
impl Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.s)
    }
}

/// A C++ fully-qualified name.
///
/// Example: `std::vector`.
#[derive(Clone, Debug)]
pub(crate) struct Path {
    components: Vec<Ident>,
}
impl From<&str> for Path {
    fn from(mut path: &str) -> Path {
        if path.starts_with("::") {
            path = &path[2..];
        }
        Path {
            components: path.split("::").map(Ident::from).collect(),
        }
    }
}
impl From<String> for Path {
    fn from(path: String) -> Path {
        From::from(path.as_str())
    }
}
impl Path {
    fn dummy() -> Path {
        Path { components: vec![] }
    }
    pub(crate) fn iter(&self) -> impl Iterator<Item = &Ident> {
        self.components.iter()
    }
}
impl Display for Path {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut iter = self.components.iter();
        let mut next = iter.next();
        while let Some(id) = next {
            write!(f, "{}", id)?;
            next = iter.next();
            if next.is_some() {
                write!(f, "::")?;
            }
        }
        Ok(())
    }
}

/// An index `Node` is the entity or set of entities referred to by a `Path`.
///
/// For example, `std` and `std::vector` are both nodes.
///
/// Nodes may contain children, and they may be represented by multiple
/// entities. Namespaces, for example, can have many AST entities associated
/// with them. Template specializations are another example.
#[derive(Debug, Default)]
pub(crate) struct Node<'tu> {
    //kind: EntityKind,
    entities: Vec<Entity<'tu>>,
    items: Option<HashMap<Ident, NodeId>>,
    inline_items: Vec<NodeId>,
}
impl<'tu> Node<'tu> {
    fn items_mut(&mut self) -> &mut HashMap<Ident, NodeId> {
        self.items.as_mut().unwrap()
    }
}

/// A lazily populated index for looking up `Node`s by name.
pub(crate) struct Index<'tu> {
    nodes: Vec<Node<'tu>>,
}
impl<'tu> Index<'tu> {
    /// Creates an index from the given TranslationUnit.
    pub(crate) fn new(file: &'tu clang::TranslationUnit<'_>) -> Index<'tu> {
        let entity = file.get_entity();
        let file_node = Node {
            //kind: entity.get_kind(),
            entities: vec![entity],
            items: None,
            inline_items: vec![],
        };
        Index {
            nodes: vec![file_node],
        }
    }

    #[inline(always)]
    pub(crate) fn node(&self, id: NodeId) -> &Node<'tu> {
        &self.nodes[id.0 as usize]
    }

    #[inline(always)]
    fn node_mut(&mut self, id: NodeId) -> &mut Node<'tu> {
        &mut self.nodes[id.0 as usize]
    }

    /// Returns the `Node` corresponding to the given `Path`.
    ///
    /// If the path does not exist, returns `LookupError::NotFound`.
    pub(crate) fn lookup(&mut self, path: &Path) -> Result<&Node<'tu>> {
        self.lookup_id(&path).map(move |id| self.node(id))
    }

    /// Returns the `NodeId` corresponding to the given `Path`.
    ///
    /// If the path does not exist, returns `LookupError::NotFound`.
    pub(crate) fn lookup_id(&mut self, path: &Path) -> Result<NodeId> {
        let mut cur = NodeId(0);
        for name in path.iter() {
            cur = match self.child_id_of(cur, name) {
                Ok(child) => child,
                // Fill in the path we failed to find.
                Err(LookupError::NotFound(_)) => return Err(LookupError::NotFound(path.clone())),
                Err(e) => return Err(e),
            }
        }
        Ok(cur)
    }

    /// Returns the child named `child` of the given `node`.
    pub(crate) fn child_of(&mut self, node: NodeId, child: &Ident) -> Result<&Node> {
        self.child_id_of(node, child).map(move |id| self.node(id))
    }

    /// Returns the `NodeId` of the child named `child` of the given `node`.
    pub(crate) fn child_id_of(&mut self, node: NodeId, child: &Ident) -> Result<NodeId> {
        if self.node(node).items.is_none() {
            self.expand(node);
        }
        let children = self.node(node).items.as_ref().unwrap();
        match children.get(child) {
            Some(id) => Ok(*id),
            None => Err(LookupError::NotFound(Path::dummy())),
        }
    }

    fn expand(&mut self, node: NodeId) -> Result<()> {
        self.node_mut(node).items = Some(HashMap::new());
        let num_entities = self.node(node).entities.len();
        for ent_idx in 0..num_entities {
            self.populate_children(node, self.node(node).entities[ent_idx])?;
        }
        Ok(())
    }

    fn populate_children(&mut self, node: NodeId, ent: Entity<'tu>) -> Result<()> {
        for child in ent.get_children() {
            let name = match ent.get_name() {
                Some(name) => Ident::from(name),
                None => continue,
            };
            let child_id = self.get_or_insert_child(node, name);
            self.node_mut(child_id).entities.push(child);
        }
        Ok(())
    }

    fn get_or_insert_child(&mut self, parent: NodeId, child: Ident) -> NodeId {
        let next_id = self.next_node_id();
        let child_id = *self
            .node_mut(parent)
            .items_mut()
            .entry(child)
            .or_insert(next_id);
        if child_id == next_id {
            self.nodes.push(Default::default());
        }
        child_id
    }

    #[inline(always)]
    fn next_node_id(&self) -> NodeId {
        NodeId(self.nodes.len() as u32)
    }
}
