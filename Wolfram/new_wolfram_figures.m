#!/afs/athena/software/math/current/bin/wolframscript -script

(*asymptote*)
s = 1/4;
eqn[x_,y_]:=s x y^4/2-y+s x;
p1 = ContourPlot[eqn[x,y]==0,{x,-3,3},{y,-5,5}, FrameStyle->White, ContourStyle->{Black}];
p2 = ContourPlot[x==0,{x,-3,3},{y,-5,5}, ContourStyle->{Dashed, Black}, FrameStyle->White];
Export["asymptote.pdf", Show[p1, p2]]
Clear[eqn]

(*ruled_hyperboloid*)
circle3D[centre_: {0, 0, 0}, radius_: 1, normal_: {0, 0, 1}, angle_: {0, 2 Pi}] :=
  Composition[
    Line,
    Map[RotationTransform[{{0, 0, 1}, normal}, centre], #] &,
    Map[Append[#, Last@centre] &, #] &,
    Append[DeleteDuplicates[Most@#], Last@#] &,
    Level[#, {-2}] &,
    MeshPrimitives[#, 1] &,
    DiscretizeRegion,
    If
  ][
    First@Differences@angle >= 2 Pi,
    Circle[Most@centre, radius],
    Circle[Most@centre, radius, angle]
  ]
p1:=Graphics3D[circle3D[{0,0,-2}], FrameStyle->White, Boxed->False];
p2:=Graphics3D[circle3D[{0,0,2}], FrameStyle->White, Boxed->False];
num:=40;
step:=2Pi/num;
scale:=17;
p3:=Graphics3D[Line[Table[{{Cos[x],Sin[x],-2}, {Cos[x+scale*step],Sin[x+scale*step],2}}, {x, 0, 2Pi, step}]], Boxed->False];
Export["ruled_hyperboloid.pdf", Show[p1, p2, p3, Boxed->False, ViewPoint->{1.3,-2.4,0}]]
