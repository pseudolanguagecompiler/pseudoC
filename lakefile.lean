package pseudoc {
  defaultFacets := #[Executable.defaultFacet]
}

lean_lib : LeanSearchPath := #[
  "(lean_packages)*/**/*"
]

executable pseudoc {
  moreLinkArgs := #["-rdynamic"]
}
