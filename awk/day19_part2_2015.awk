#!/usr/bin/awk -f
BEGIN{
  filename="input.txt"
  found_blank=0
  while ((getline line < filename) > 0) {
    if (found_blank && !found_molecule) {
      molecule=line
      total=0; rn=0; ar=0; y=0
      for (i=1; i<=length(molecule); ) {
        c=substr(molecule,i,1)
        if (c ~ /[A-Z]/) {
          total++
          if (i<length(molecule) && substr(molecule,i+1,1) ~ /[a-z]/) {
            elem=substr(molecule,i,2)
            i+=2
          } else {
            elem=c
            i++
          }
          if (elem=="Rn") rn++
          else if (elem=="Ar") ar++
          else if (elem=="Y") y++
        } else {
          i++
        }
      }
      steps=total - rn - ar - 2*y - 1
      print steps
      exit
    }
    if (line=="") found_blank=1
  }
}