use std::fmt::Debug;

use crate::language::Language;

pub trait Behavior<L: Language>: Debug {
    fn kind(&self) -> L::BehaviorKind;
}
