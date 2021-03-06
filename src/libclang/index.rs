// Copyright (c) 2021 Google LLC
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// https://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or https://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! A lazily populated index for looking up entities by name.

use crate::ir::bindings::{Ident, Path, PathComponent};
use clang::{self, Entity, EntityKind};
use std::collections::HashMap;

/// Represents errors which can occur while looking up entities in the index.
#[derive(Debug)]
pub enum LookupError {
    NotFound(Path),
}

/// The result of a lookup operation.
type Result<T> = std::result::Result<T, LookupError>;

/// An ID assigned to a `Path` in the index.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct NodeId(u32);

impl NodeId {
    #[inline(always)]
    fn from_usize(n: usize) -> NodeId {
        NodeId(n as u32)
    }
    #[allow(dead_code)]
    #[inline(always)]
    fn as_usize(&self) -> usize {
        self.0 as usize
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
pub struct Node<'tu> {
    //kind: EntityKind,
    pub entities: Vec<Entity<'tu>>,
    items: Option<HashMap<PathComponent, NodeId>>,
    inline_items: Vec<NodeId>,
}
impl<'tu> Node<'tu> {
    fn items_mut(&mut self) -> &mut HashMap<PathComponent, NodeId> {
        self.items.as_mut().unwrap()
    }
}

/// A lazily populated index for looking up `Node`s by name.
pub struct PathIndex<'tu> {
    nodes: Vec<Node<'tu>>,
}
impl<'tu> PathIndex<'tu> {
    /// Creates an index from the given TranslationUnit.
    pub fn new(file: &'tu clang::TranslationUnit<'_>) -> PathIndex<'tu> {
        let entity = file.get_entity();
        let file_node = Node {
            //kind: entity.get_kind(),
            entities: vec![entity],
            items: None,
            inline_items: vec![],
        };
        PathIndex {
            nodes: vec![file_node],
        }
    }

    #[inline(always)]
    pub fn node(&self, id: NodeId) -> &Node<'tu> {
        &self.nodes[id.0 as usize]
    }

    #[inline(always)]
    fn node_mut(&mut self, id: NodeId) -> &mut Node<'tu> {
        &mut self.nodes[id.0 as usize]
    }

    /// Returns the `Node` corresponding to the given `Path`.
    ///
    /// If the path does not exist, returns `LookupError::NotFound`.
    pub fn lookup(&mut self, path: &Path) -> Result<&Node<'tu>> {
        self.lookup_id(&path).map(move |id| self.node(id))
    }

    /// Returns the `NodeId` corresponding to the given `Path`.
    ///
    /// If the path does not exist, returns `LookupError::NotFound`.
    pub fn lookup_id(&mut self, path: &Path) -> Result<NodeId> {
        let mut cur = NodeId(0);
        for (idx, name) in path.iter().enumerate() {
            cur = match self.child_id_of(cur, name)? {
                Some(child) => child,
                None => {
                    let err_path = path.iter().take(idx + 1).cloned().collect::<Path>();
                    return Err(LookupError::NotFound(err_path));
                }
            };
        }
        Ok(cur)
    }

    /// Returns the child named `child` of the given `node`.
    #[allow(dead_code)]
    pub fn child_of(&mut self, node: NodeId, child: &PathComponent) -> Result<Option<&Node>> {
        self.child_id_of(node, child)
            .map(|opt| opt.map(move |id| self.node(id)))
    }

    /// Returns the `NodeId` of the child named `child` of the given `node`.
    pub fn child_id_of(&mut self, node: NodeId, child: &PathComponent) -> Result<Option<NodeId>> {
        if self.node(node).items.is_none() {
            self.expand(node)?;
        }
        let children = self.node(node).items.as_ref().unwrap();
        Ok(children.get(child).copied())
    }

    fn expand(&mut self, node: NodeId) -> Result<()> {
        self.node_mut(node).items = Some(HashMap::new());
        let num_entities = self.node(node).entities.len();
        for ent_idx in 0..num_entities {
            let ent = self.node(node).entities[ent_idx];

            // TODO: Looking up children of unsupported entity types will result
            // in confusing NotFound errors.
            if should_expand(ent) {
                self.populate_children(node, ent)?;
            }
        }
        Ok(())
    }

    fn populate_children(&mut self, parent: NodeId, ent: Entity<'tu>) -> Result<()> {
        for child in ent.get_children() {
            let name = match child.get_name() {
                Some(name) => PathComponent::from(Ident::from(name)),
                None => continue,
            };
            let child_id = self.get_or_insert_child(parent, name);
            self.node_mut(child_id).entities.push(child);

            if should_inline(child) {
                //self.node_mut(parent).inline_items.push(child_id);
                // Populate the parent with all of this node's children.
                self.populate_children(parent, child)?;
            }
        }
        Ok(())
    }

    fn get_or_insert_child(&mut self, parent: NodeId, child: PathComponent) -> NodeId {
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
        NodeId::from_usize(self.nodes.len())
    }
}

fn should_expand(ent: Entity<'_>) -> bool {
    use EntityKind::*;
    match ent.get_kind() {
        TranslationUnit => true,
        Namespace | StructDecl | ClassDecl | ClassTemplate => true,
        _ => false,
    }
}

fn should_inline(ent: Entity<'_>) -> bool {
    use EntityKind::*;
    match ent.get_kind() {
        Namespace => ent.is_inline_namespace(),
        EnumDecl => !ent.is_scoped(),
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn path(p: &str) -> Path {
        p.split("::")
            .map(Ident::from)
            .map(PathComponent::from)
            .collect()
    }

    #[test]
    fn inline_namespace() {
        let clang = crate::libclang::clang();
        let index = clang::Index::new(&clang, true, true);
        let file = cpp_parse!(&index, {
            namespace std {
                inline namespace __1 {
                    struct vector {};
                }
                namespace notinline {
                    struct foo {};
                }
            }
        });
        let mut index = PathIndex::new(&file);
        assert!(index.lookup(&path("std::__1::vector")).is_ok());
        assert!(index.lookup(&path("std::vector")).is_ok());
        assert!(index.lookup(&path("std::notinline::foo")).is_ok());
        assert!(index.lookup(&path("std::foo")).is_err());
    }
}
