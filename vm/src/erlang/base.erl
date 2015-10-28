-module(base).
-export([lookup/2,fst/1,snd/1,show/1,print/1,putStr/1,putStrLn/1,nth/2,subList/3,limit/2]).

lookup(_Key,[]) -> nothing;
lookup(Key,[{Key,Value}|_]) -> {just,Value};
lookup(Key,[_|Xs]) -> lookup(Key,Xs).

fst({X,_Y}) -> X.

snd({_X,Y}) -> Y.

show(X) when is_atom(X)    -> atom_to_list(X);
show(X) when is_integer(X) -> integer_to_list(X);
show(X) when is_pid(X)     -> pid_to_list(X);
show(X) when is_tuple(X)   -> "{"++showList(tuple_to_list(X))++"}";
show(X) when is_list(X)    ->
  case lists:all(fun(C)->(C>7) and (C<127) end,X) of
    true -> X;
    false -> "["++showList(X)++"]"
  end.

showList([]) -> "";
showList([X|Xs]) -> show(X)++case Xs of
                               [] -> "";
                               _ -> ","++showList(Xs)
                             end.


print(X) -> io:format("~s~n",[show(X)]).

putStr(Str) ->  io:format("~s",[Str]).

putStrLn(Str) -> putStr(Str), putStr("\n").

nth(0,_) -> throw('no position 0 defined for lists');
nth(_,[]) -> throw('no element at this position');
nth(P,[X|Xs]) -> case P of
                   1 -> X;
                   _ -> nth(P-1,Xs)
                 end.

subList(0,_,_) -> throw('no position 0 defined for lists');
subList(1,L,Xs) -> limit(L,Xs);
subList(N,L,[_|Xs]) -> subList(N-1,L,Xs).

limit(0,_) -> [];
limit(_,[]) -> [];
limit(N,[X|Xs]) -> [X|limit(N-1,Xs)].