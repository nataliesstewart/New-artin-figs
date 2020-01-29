#!/afs/athena/software/math/current/bin/wolframscript -script

(*asymptote*)
s = 1/8;
eqn[x_,y_]:=s x y^4/2-y+s x;
p1 = ContourPlot[eqn[x,y]==0,{x,-5,5},{y,-10,10}, FrameStyle->White, ContourStyle->{Black}];
p2 = ContourPlot[x==0,{x,-5,5},{y,-10,10}, ContourStyle->{Dashed, Black}, FrameStyle->White];
Export["images/Wolfram/asymptote.pdf", Show[p1, p2]]
Clear[eqn]

(*capricornoid*)
eqn[a_,b_][x_,y_]:=a^2x^2(x^2+y^2)-b(a y-x^2-y^2)^2;
p1 := ContourPlot[(eqn[2,8][x,y]==0) // Evaluate,{x,-2.4,2.4},{y,-.1,2.5},Axes->False,AspectRatio->Automatic,MaxRecursion->6,ImageSize->Full,FrameStyle->White, ContourStyle->{Black}];
p2 := Plot[0,{x,-0.05,0.05},Axes->False, ImageSize->Full,FrameStyle->White, PlotStyle->{Black}];
Export["images/Wolfram/capricornoid.pdf", Show[p1, p2]]
Clear[eqn]

(*cubic*)
eqn[x_,y_]:= x^3-(1/2)x^2-x+1/2-y^2;
Export["images/Wolfram/cubic.pdf", ContourPlot[(eqn[x,y]==0) // Evaluate,{x,-2.4,2.4},{y,-3.5,3.5},AspectRatio->1,MaxRecursion->6,Axes->False,FrameStyle->White, ContourStyle->{Black}]]
Clear[eqn]

(*parametrized_cusp*)
eqn[x_,y_] := x^3-y^2;
Export["images/Wolfram/parametrized_cusp.pdf", ContourPlot[eqn[x,y]==0,{x,-1,1},{y,-1,1},Axes->False,FrameStyle->White,ImageSize->Full, ContourStyle->{Black}]]
Clear[eqn]

(*double_torus*)
eqn[x_,y_]:=(y^2+(x-1)^2-1)(y^2+(x+1)^2-1)-0.1;
Export["images/double_torus/pre_double_torus.pdf", ContourPlot[eqn[x,y]==0,{x,-3,3},{y,-4,4}, ContourStyle->Black, Axes->False, Frame->False]]
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
num:=24;
step:=2Pi/num;
scale:=10;
p3:=Graphics3D[Line[Table[{{Cos[x],Sin[x],-2}, {Cos[x+scale*step],Sin[x+scale*step],2}}, {x, 0, 2Pi, step}]], Boxed->False];
Export["images/Wolfram/ruled_hyperboloid.pdf", Show[p1, p2, p3, Boxed->False, ViewPoint->{1.3,-2.4,0}]]

(*trott_curve_tangents*)
eqn:=12^2(x^4+y^4)-15^2(x^2+y^2)+350x^2y^2+81;
Export["images/Wolfram/trott_curve_tangents.pdf", ContourPlot[{eqn\[Equal]0,x+0.445,y-0.708,y==x+1.22},{x,-1,1},{y,-1,1},ContourStyle->{{Black},{Gray,Dashed},{Gray,Dashed},{Gray,Dashed}},ImageSize->200, Frame->False]]
Clear[eqn]

(*parametrized_node*)
eqn[x_,y_]:= x^3-(1/2)x^2-x+1/2-y^2-(35-13Sqrt[13])/108;
Export["images/Wolfram/parametrized_node.pdf", ContourPlot[eqn[x,y]==0,{x,-3,3},{y,-3,3},Axes->False,FrameStyle->White,ImageSize->Full, ContourStyle->{Black}]]
Clear[eqn]

(*parametrized_tacnode*)
p1:=ContourPlot[y-x^2==0, {x,-1,1}, {y,-1,1}, Axes->False, FrameStyle->White, ImageSize->Full, ContourStyle->{Black}];
p2:=ContourPlot[y+x^2==0, {x,-1,1}, {y,-1,1}, Axes->False, FrameStyle->White, ImageSize->Full, ContourStyle->{Black}];
Export["images/Wolfram/parametrized_tacnode.pdf", Show[p1, p2]]
Clear[eqn]
