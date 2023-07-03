
# make sure that loader can find libqcdloop1.so and libmwrap_qcdloop1.so
# e.g. by > export LD_LIBRARY_PATH=/usr/local/lib

wrapperpath:="libmwrap_qcdloop1.so";

qlinit:=define_external('MWRAP_qlinit_','MAPLE','LIB'=wrapperpath);
qlinit();
qlI1:=define_external('MWRAP_qli1_','MAPLE','LIB'=wrapperpath);
qlI2:=define_external('MWRAP_qli2_','MAPLE','LIB'=wrapperpath);
qlI3:=define_external('MWRAP_qli3_','MAPLE','LIB'=wrapperpath);
qlI4:=define_external('MWRAP_qli4_','MAPLE','LIB'=wrapperpath);

musq:=1.1;
m1sq:=3;
m2sq:=5;
m3sq:=9;
m4sq:=1.3;
p1sq:=1.5;
p2sq:=-1.7;
p3sq:=2.3;
p4sq:=2.9;
s12:=37;
s23:=-15.7;

for ep from -2 to 0 do print("ep,qlI1(m1sq,musq,ep)",ep,qlI1(m1sq,musq,ep)); od;
for ep from -2 to 0 do print("ep,qlI2(p1sq,m1sq,m2sq,musq,ep)",ep,qlI2(p1sq,m1sq,m2sq,musq,ep)); od;
for ep from -2 to 0 do print("ep,qlI3(p1sq,p2sq,p3sq,m1sq,m2sq,m3sq,musq,ep)",ep,qlI3(p1sq,p2sq,p3sq,m1sq,m2sq,m3sq,musq,ep)); od;
for ep from -2 to 0 do print("ep,qlI4(p1sq,p2sq,p3sq,p4sq,s12,s23,m1sq,m2sq,m3sq,m4sq,musq,ep)",ep,qlI4(p1sq,p2sq,p3sq,p4sq,s12,s23,m1sq,m2sq,m3sq,m4sq,musq,ep)); od;
print("test of divergent boxes");
for ep from -2 to 0 do print("ep,qlI4(0d0,0d0,p3sq,p4sq,s12,s23,0d0,0d0,0d0,m4sq,musq,ep)",ep,qlI4(0,0,p3sq,p4sq,s12,s23,0,0,0,m4sq,musq,ep)); od;
