
/* REXX */
CALL main
EXIT

main:
  NUMERIC DIGITS 64
  fname = 'input.txt'
  CALL STREAM fname, 'c', 'OPEN READ'
  DO i = 0 TO 2
    CALL ReadLine LINEIN(fname), i
  END
  CALL STREAM fname, 'c', 'CLOSE'

  s0_p = hail.0.p.x hail.0.p.y hail.0.p.z
  s0_v = hail.0.v.x hail.0.v.y hail.0.v.z
  s1_p = hail.1.p.x hail.1.p.y hail.1.p.z
  s1_v = hail.1.v.x hail.1.v.y hail.1.v.z
  s2_p = hail.2.p.x hail.2.p.y hail.2.p.z
  s2_v = hail.2.v.x hail.2.v.y hail.2.v.z

  ref1_p = VecSubtract(s1_p, s0_p)
  ref1_v = VecSubtract(s1_v, s0_v)
  ref2_p = VecSubtract(s2_p, s0_p)
  ref2_v = VecSubtract(s2_v, s0_v)

  t1 = IntersectionTime(ref2_p, ref2_v, ref1_p, ref1_v)
  t2 = IntersectionTime(ref1_p, ref1_v, ref2_p, ref2_v)

  rock1 = VecAdd(s1_p, VecMultiply(s1_v, t1))
  rock2 = VecAdd(s2_p, VecMultiply(s2_v, t2))

  rock_v = VecDivide(VecSubtract(rock2, rock1), t2 - t1)
  rp = VecSubtract(rock1, VecMultiply(rock_v, t1))

  PARSE VAR rp rpx rpy rpz
  SAY TRUNC(rpx + rpy + rpz)
RETURN

ReadLine: PROCEDURE EXPOSE hail.
  ARG line, i
  line = STRIP(TRANSLATE(line, ' ', '@,'))
  PARSE VAR line hail.i.p.x hail.i.p.y hail.i.p.z hail.i.v.x hail.i.v.y hail.i.v.z
RETURN

VecAdd: PROCEDURE
  ARG v1_str, v2_str
  PARSE VAR v1_str v1x v1y v1z
  PARSE VAR v2_str v2x v2y v2z
  RETURN (v1x + v2x) (v1y + v2y) (v1z + v2z)

VecSubtract: PROCEDURE
  ARG v1_str, v2_str
  PARSE VAR v1_str v1x v1y v1z
  PARSE VAR v2_str v2x v2y v2z
  RETURN (v1x - v2x) (v1y - v2y) (v1z - v2z)

VecMultiply: PROCEDURE
  ARG v_str, s
  PARSE VAR v_str vx vy vz
  RETURN (vx * s) (vy * s) (vz * s)

VecDivide: PROCEDURE
  ARG v_str, s
  PARSE VAR v_str vx vy vz
  RETURN (vx / s) (vy / s) (vz / s)

VecCross: PROCEDURE
  ARG v1_str, v2_str
  PARSE VAR v1_str v1x v1y v1z
  PARSE VAR v2_str v2x v2y v2z
  rx = v1y * v2z - v1z * v2y
  ry = v1z * v2x - v1x * v2z
  rz = v1x * v2y - v1y * v2x
  RETURN rx ry rz

VecDot: PROCEDURE
  ARG v1_str, v2_str
  PARSE VAR v1_str v1x v1y v1z
  PARSE VAR v2_str v2x v2y v2z
  RETURN v1x * v2x + v1y * v2y + v1z * v2z

IntersectionTime: PROCEDURE
  ARG r_p_str, r_v_str, s_p_str, s_v_str
  plane = VecCross(r_p_str, r_v_str)
  num = -VecDot(s_p_str, plane)
  den = VecDot(s_v_str, plane)
  RETURN num / den
