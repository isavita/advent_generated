import Lake

open Lake DSL

package "aoc" where

target md5Obj pkg : System.FilePath := do
  let oFile := pkg.buildDir / "c" / "md5.o"
  let srcJob ← inputTextFile <| pkg.dir / "md5.c"

  buildO oFile srcJob #[
    "-I", (← getLeanIncludeDir).toString,
    "-I", "/opt/homebrew/opt/openssl@3/include",
    "-O3",
    "-fPIC"
  ]

lean_exe day4_part1_2015 where
  root := `day4_part1_2015
  moreLinkObjs := #[md5Obj]
  moreLinkArgs := #[
    "-L/opt/homebrew/opt/openssl@3/lib",
    "-lcrypto"
  ]

lean_exe day4_part2_2015 where
  root := `day4_part2_2015
  moreLinkObjs := #[md5Obj]
  moreLinkArgs := #[
    "-L/opt/homebrew/opt/openssl@3/lib",
    "-lcrypto"
  ]
