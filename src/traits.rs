pub enum TermKind<Var> {
    Var(Var),
    NonVar,
}

pub trait ClassifyTerm<Var> {
    fn classify_term(&self) -> TermKind<&Var>;

    /// Without looking at any children of `self` or `other`, determine whether
    /// or not they represent a unifiable pair of terms.
    ///
    /// Note that this is a different distinction than that of `classify_term`. Two values
    /// could be `!superficially_unifiable` but have the same `TermKind`.
    fn superficially_unifiable(&self, other: &Self) -> bool;

    fn is_var(&self) -> bool {
        matches!(self.classify_term(), TermKind::Var(_))
    }

    fn is_non_var(&self) -> bool {
        matches!(self.classify_term(), TermKind::NonVar)
    }
}

pub trait DirectChildren<Var>: ClassifyTerm<Var> + Sized
where
    Var: Clone,
{
    /// All *direct* children (and *only* the *direct* children) of `Self` which are of
    /// type `Self` should be yielded.
    fn direct_children<'a>(&'a self) -> Box<dyn Iterator<Item = Self> + 'a>;

    /// Given a function, you need to create a new `Self` where your *direct* children
    /// have been replaced with the ones provided by the function.
    fn map_direct_children(&self, f: impl FnMut(&Self) -> Self) -> Self;

    /// Returns an iterator of all the the variables contained in `self`. If `self` is a
    /// variable, returns itself.
    /// ## Duplicate Variables
    /// This iterator is *not* de-duplicated. Duplicate variables may appear. You may want
    /// to `collect` this iterator into a set to deduplicate elements.
    fn variables<'s: 'it, 'it>(&'s self) -> Box<dyn Iterator<Item = Var> + 'it>
    where
        Var: 's,
    {
        if let TermKind::Var(v) = self.classify_term() {
            Box::new(std::iter::once(v.clone()))
        } else {
            Box::new(
                self.direct_children()
                    .flat_map(|child| child.variables().collect::<Vec<_>>()),
            )
        }
    }
}
