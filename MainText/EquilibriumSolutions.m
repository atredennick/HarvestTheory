%Script to solve for equilibrium solutions for the Staver forest-savanna
%model with tree harvest

syms o p a u v b w F G S T
solsForest = solve('(o+p)*F-a*G*F=0', a) 
solsSav = solve('b*G*T+p*T-w*S-u*S=0', 'w*S-T*(v+p)=0', S, w)

%Check to make sure Staver equilibriums can be recovered
solStaver = solve('b*G*T-w*S-u*S=0', 'w*S-v*T=0', S, w)

%print out equilibriums
solsForest
solsSav.w
solStaver.w

%calculate derivatives with respect to G for stability condition
savannaDeriv = diff(solsSav.w, G, 1)

%check against Staver solution
diff(solStaver.w, G, 1)

%print Savanna (GST) derivative
savannaDeriv
