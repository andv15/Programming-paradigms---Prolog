/*
 * Compact flows are represented as lists of bindings, and each binding
 * is actually a list of two elements, the first being the field name
 * and the second - the expected value or the 'any' atom.
 *
 * A special compact flow is 'cvoid' - the null compact flow.
 * While the rest of the compact flows are lists with the length equal
 * with the number of fields in the problem, the null flow is only this
 * atom.
 *
 * Flows are represented by lists meaning reunions of compact flows.
 *
 * Example 1:
 * The flow [[[src, 1], [dst, 3]], [[src, 2], [dst, 3]]]
 * means the reunion of the two compact flows, each
 * having the destination field set to 3.
 *
 * Example 2:
 * The flow []
 * is the null flow.
 *
 * Please manually compile Prolog if the PlUnit module is not available
 * under linux (uninstall the one from the package manager first to be
 * sure it doesn't trigger any conflicts).
 *
 *  git clone git://prolog.cs.vu.nl/home/pl/git/pl.git
 *  cd pl
 *  ./configure # Answer 'yes' at the first question, then '3' at the second
 *  make
 *  sudo make install
 *
 * And then, inside the interpreter:
 * :- ['tema.pl', 'checker.plt']
 * :- run_tests.
 *
 * 
 * Under Windows, the SWI-Prolog program has PlUnit built-in.
 */


/*
 * setCf(+cflow, +header_name, +value, -cflow_out)
 *
 * Overwrite a field value in a compact flow
 */
setCf([], _, _, []).
setCf([[H,_]|FI], H, V, [[H,V]|FO]) :- setCf(FI, H, V, FO).
setCf([[HI,VI]|FI], H, V, [[HI,VI]|FO]) :- dif(HI, H), setCf(FI, H, V, FO).

/*
 * set(+flow, +header_name, +value, -flow_out)
 *
 * Overwrite a field value in a flow
 */
set([], _, _, []).
set([HI|FI], H, V, [HO|FO]):- apply(setCf, [HI, H, V, HO]), set(FI, H, V, FO), !.

/******************** Misc list functions ******************************/
/*
 * rmDup(+in_list, -out_list)
 *
 * Removes duplicates from a list. 
 *
 * The membership test to the rest of the set is the one present in the member/2
 * implementation.
 * The cut below is to simplify the tests (no choicepoint means deterministic testing).
 */
rmDup([], []).
rmDup([H|T], R) :- member(H,T), rmDup(T,R), !.
rmDup([H|T], [H|R]) :- rmDup(T,R), !.

/*
 * rmVal(+in_list, +in_term, -out_list)
 *
 * Removes all instances of the in_term from the in_list
 */
rmVal([], _, []).
rmVal([H|FI], T, [H|FO]) :- dif(H,T), rmVal(FI, T, FO), !.
rmVal([H|FI], H, FO) :- rmVal(FI, H, FO), !.


/******************** Intersection ******************************/
/*
 * intCf(+flow1, +flow2, -out)
 *
 * Compact flow intersection. If the flows are disjoint, the
 * 'cvoid' atom is returned.
 */
intCf(cvoid, _, cvoid).
intCf(_, cvoid, cvoid).
intCf([],[],[]).
intCf([[H,V]|T1], [[H,V]|T2], [[H,V]|R]) :- intCf(T1, T2, R), !.
intCf([[H,V]|T1], [[H,any]|T2], [[H,V]|R]) :- intCf(T1, T2, R), !.
intCf([[H,any]|T1], [[H,V]|T2], [[H,V]|R]) :- intCf(T1, T2, R), !.
intCf([[H,V1]|_], [[H,V2]|_], R) :- dif(V1,V2), R=cvoid, !.


/*
 * inter(+flow1, +flow2, -flow_out)
 *
 * Flow intersection.
 * If the flows are disjoint, the [] is returned.
 */
inter([], _, []).
inter(_, [], []).
inter([H1|_], [H2|_], []) :- intCf(H1, H2, R), R==cvoid, !.
inter([H1|F1], [H2|F2], [HR|FR]) :- intCf(H1, H2, HR), dif(HR, cvoid), inter(F1, F2, FR), !.
inter([H1|F1], [H2|F2], FR) :- intCf(H1, H2, HR), HR==cvoid, inter(F1, F2, FR), !.

/******************** Overwriting ******************************/
/*
 * modify(+flow_in, +new_bindings, -flow_out)
 *
 * Overwrite the bindings to the new values.
 * This will be used in implementing ovr network functions
 */
modify(_, [], []).
modify(FI,[[H,V]], R) :- set(FI, H, V, R), !.
modify(FI, [[H,V]|T], R) :- set(FI, H, V, FO), modify(FO, T, R), !.

/******************** Subset ******************************/
/*
 * subsetCf(+cflow1, +cflow2)
 *
 * This predicate returns true if cflow1 is a subset of cflow2
 */
subsetCf([], []) :- !.
subsetCf([[H,V]|F1], [[H,V]|F2]) :- subsetCf(F1, F2), !.
subsetCf([[H,_]|F1], [[H,any]|F2]) :- subsetCf(F1, F2), !.

/*
 * subset(+flow1, +flow2)
 *
 * This predicate returns true if flow2 fully contains flow1.
 */
subsetFlow([], []) :- !.
subsetFlow([H1|F1], [H2|F2]) :- subsetFlow(F1, F2), !, subsetCf(H1, H2).
subsetFlow([H1|_], [H2|_]) :- subsetFlow(H1, H2), !.



/******************** Rule generation *************************/

/*
 * genericRule(M, F, O).
 *
 * A basic unit of our model. These may be instantiated manually
 * (as in the test below) or being asserted from a wireRule.
 *
 * The tree parameters of a generic rule are
 * Match - for a rule to match an inbound flow, the flow
 *		has to be a subset of this parameter (also a flow).
 *
 * Filter -  after a rule matches, the inbound flow is intersected
 *		with the filter flow, so that some compact flows may be dropped
 *
 * Ovr - a filtered flow should then have some values overwritten.
 *
 * ------------
 * In this example we are using two fields for each flow:
 * port - the physical port where the packet is placed
 * dst - the destination of the packet
 */

/*
 * A wireRule(R1, R2) receives two ports (physical endpoints)
 * and matches the traffic coming to the first endpoint (R1)
 * and overwrites the field 'port' to R2, representing the
 * traffic on the other endpoint.
 *
 * wireRules are instantiated by the testing suite.
 */

/*
 * Write a predicate to deduce a genericRule from a wireRule
 */

genericRule(M, F, O) :- wireRule(P1, P2), set([[[port, any],[dst, any]]], port, P1, M),
			set([[[port, any],[dst, any]]], port, P1, F),
			setCf([[port, P1]], port, P2, O).

/*
 * nf(+flow_in, -flow_out)
 *
 * Apply a matching network function on the flow and output the results.
 */
nf(FI, FO) :- genericRule(M, F, O), subsetFlow(FI, M), inter(FI, F, INTERF), dif(INTERF, []), modify(INTERF, O, FO), !. 
nf(FI, []) :- genericRule(_, M, _), not(subsetFlow(FI, M)). 

/******************** Reachability *************************/
/*
 * reach(+src, -out, aux)
 *
 * The 'aux' term of the predicate may be used in your implementation
 * as you wish. The initial 'reach' call will set it to the empty list.
 *
 * The 'out' term should return, each time, a flow which is derived by applying
 * network functions over the input flow or over other results.
 */

reach(S, [], _) :- nf(S, R), R==[], !.
reach(S, [], [S, N]) :- dif(N, 0), !.
reach(S, [R|O], [S, N]) :- N==0, nf(S, R), reach(R, O, [S, N+1]), !.
reach(S, [R|O], [A, N]) :- nf(S, R), reach(R, O, [A, N]), dif(R, []), !.


/*
 * reachAll(+src, -out)
 *
 * Compute all the reachable flows starting with the reachAll.
 *
 * hint: findAll
 */
reachAll(FI, FO) :- reach(FI, FO, [FI, 0]).

/*
 * Loop detect
 *
 * Returns 'true' if there's a cyclic path inside the connected
 * component of the graph that contains the flow
 *
 * Bonus task,
 */
loop(FI) :- reachAll(FI, FO), member(FI, FO).

/*
 *
 * DO NOT EDIT THE FILE BELOW THIS POINT
 *
 */

:- style_check(-discontiguous).

/******************** Ring sample network *************************/
wireRule(p1, p2).
wireRule(p2, p3).
wireRule(p3, p1).

/******************** Bus sample network *************************/
wireRule(p10, p11).
wireRule(p11, p12).
wireRule(p12, p13).

/******************** Star sample network *************************/
wireRule(p20, p21).
wireRule(p20, p22).
wireRule(p20, p23).
wireRule(p20, p24).

/******************** Firewalled server *************************/
wireRule(p30, p31).
genericRule([[[port, p31], [dst, any]]], [[[port, any], [dst, p33]]], [[port, p32]]).
wireRule(p32, p33).

/******************** Proxy demo *************************/
wireRule(p40, p41).
genericRule([[[port, p41], [dst, any]]], [[[port, any], [dst, p41]]], [[port, p42], [dst, p49]]).
wireRule(p42, p49).

:- load_test_files([]).
