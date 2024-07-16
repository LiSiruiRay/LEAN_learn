import Lake
open Lake DSL

package «lean_learn» where
  -- add package configuration options here

lean_lib «LeanLearn» where
  -- add library configuration options here

@[default_target]
lean_exe «lean_learn» where
  root := `Main
