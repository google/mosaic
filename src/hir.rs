//! "High-level IR", which right now is a unified index that resolves to
//! clang::Entity.

#![allow(unused)]

use clang::{self, Entity, EntityKind};
use std::collections::HashMap;
use std::fmt::{self, Display};
use thiserror::Error;

#[derive(Error, Debug)]
pub(crate) enum LookupError {
    #[error("couldn't find `{0}`")]
    NotFound(RevPath),
    #[error("{0}")]
    SourceError(clang::SourceError),
}
type Result<T> = std::result::Result<T, LookupError>;

#[derive(Clone, Copy, Debug)]
pub(crate) struct NodeId(u32);

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub(crate) struct Ident {
    s: String,
}
impl From<&str> for Ident {
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
    fn iter(&self) -> impl Iterator<Item = &Ident> {
        self.components.iter()
    }
}
impl Display for Path {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write_path(self.iter(), f)
    }
}

// A path stored in reverse; only used in LookupError::NotFound.
#[derive(Clone, Debug)]
pub(crate) struct RevPath {
    rev_components: Vec<Ident>,
}
impl RevPath {
    fn new() -> RevPath {
        RevPath {
            rev_components: vec![],
        }
    }
    fn push_front(&mut self, id: Ident) {
        self.rev_components.push(id);
    }
}
impl Display for RevPath {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write_path(self.rev_components.iter().rev(), f)
    }
}

fn write_path<'a>(
    mut iter: impl Iterator<Item = &'a Ident>,
    f: &mut fmt::Formatter,
) -> fmt::Result {
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

pub(crate) struct HirNode<'tu> {
    kind: EntityKind,
    entities: Vec<Entity<'tu>>,
    items: Option<HashMap<Ident, NodeId>>,
    inline_items: Vec<NodeId>,
}

pub(crate) struct Hir<'tu> {
    nodes: Vec<HirNode<'tu>>,
}
impl<'tu> Hir<'tu> {
    pub(crate) fn new(file: &'tu clang::TranslationUnit<'_>) -> Hir<'tu> {
        let entity = file.get_entity();
        let file_node = HirNode {
            kind: entity.get_kind(),
            entities: vec![entity],
            items: None,
            inline_items: vec![],
        };
        Hir {
            nodes: vec![file_node],
        }
    }

    pub(crate) fn node(&self, id: NodeId) -> &HirNode<'tu> {
        &self.nodes[id.0 as usize]
    }

    fn node_mut(&mut self, id: NodeId) -> &mut HirNode<'tu> {
        &mut self.nodes[id.0 as usize]
    }

    pub(crate) fn lookup(&mut self, path: &Path) -> Result<&HirNode<'tu>> {
        self.lookup_id(&path).map(move |id| self.node(id))
    }

    pub(crate) fn lookup_id(&mut self, path: &Path) -> Result<NodeId> {
        let mut cur = NodeId(0);
        for name in path.iter() {
            cur = self.child_of(cur, name)?;
        }
        Ok(cur)
    }

    pub(crate) fn child_of(&mut self, node: NodeId, child: &Ident) -> Result<NodeId> {
        if self.node(node).items.is_none() {
            self.expand(node);
        }
        let children = self.node(node).items.as_ref().unwrap();
        match children.get(child) {
            Some(id) => Ok(*id),
            None => Err(LookupError::NotFound(RevPath::new())),
        }
    }

    fn expand(&mut self, node: NodeId) -> Result<()> {
        self.node_mut(node).items = Some(HashMap::new());
        let num_entities = self.node(node).entities.len();
        for ent_idx in 0..num_entities {
            self.lower(node, self.node(node).entities[ent_idx])?;
        }
        Ok(())
    }

    fn lower(&mut self, node: NodeId, ent: Entity<'tu>) -> Result<NodeId> {
        todo!()
    }
}
