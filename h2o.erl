-module(h2o).
-export([start/0, oxigenio/1, hidrogenio/1, geradorDeMoleculas/1, mediador/2]).

geradorDeMoleculas(Mediador_PID) ->
    timer:sleep(round(timer:seconds(10))),
    X = rand:uniform(10),
    if
        X >= 6 -> 
            spawn(h2o, oxigenio, [Mediador_PID]);
        X =< 5 ->
            spawn(h2o, hidrogenio, [Mediador_PID])
    end,
    geradorDeMoleculas(Mediador_PID).

hidrogenio(Mediador_PID) ->
    Hidrogenio_PID = self(),
    io:format("Gerado molecula número ~s de Hidrogênio~n", [pid_to_list(Hidrogenio_PID)]),
    timer:sleep(round(timer:seconds(rand:uniform(20)+10))),
    Mediador_PID ! {hidrogenio, self()},
    receive
        kill ->
            exit(shutdown)
    end.


oxigenio(Mediador_PID) ->
    Oxigenio_PID = self(),
    io:format("Gerado molecula número ~s de Oxigênio~n", [pid_to_list(Oxigenio_PID)]),
    timer:sleep(round(timer:seconds(rand:uniform(20)+10))),
    Mediador_PID ! {oxigenio, self()},
    receive
        kill ->
            exit(shutdown)
    end.
    
mediador(Lista_PID_Oxigenio, Lista_PID_Hidrogenio) ->
    if 
        length(Lista_PID_Hidrogenio) >= 2 andalso length(Lista_PID_Oxigenio) >= 1 ->
            {Hidrogenio_PID_1, Lista_PID_Hidrogenio_1} = remove_elemento_lista(1, Lista_PID_Hidrogenio),
            {Hidrogenio_PID_2, Lista_PID_Hidrogenio_2} = remove_elemento_lista(1, Lista_PID_Hidrogenio_1),
            {Oxigenio_PID_1, Lista_PID_Oxigenio_1} = remove_elemento_lista(1, Lista_PID_Oxigenio),
            io:format("Formado molecula H2O com elementos: (~p + ~p + ~p).~n", [Hidrogenio_PID_1, Hidrogenio_PID_2, Oxigenio_PID_1]),
            Hidrogenio_PID_1 ! kill,
            Hidrogenio_PID_2 ! kill,
            Oxigenio_PID_1 ! kill,
            mediador(Lista_PID_Oxigenio_1, Lista_PID_Hidrogenio_2);
        true ->
            io:format("")
    end,
    receive
        {oxigenio, Oxigenio_PID} ->
            io:format("Molecula de Oxigênio númer ~p está com energia suficiente~n", [pid_to_list(Oxigenio_PID)]),
            Lista_PID_Oxigenio_Nova = [ Oxigenio_PID | Lista_PID_Oxigenio ],
            mediador(Lista_PID_Oxigenio_Nova, Lista_PID_Hidrogenio);
        {hidrogenio, Hidrogenio_PID} ->
            Lista_PID_Hidrogenio_Nova = [ Hidrogenio_PID | Lista_PID_Hidrogenio ],
            io:format("Molecula de Hidrogênio número ~p está com energia suficiente!~n", [Hidrogenio_PID]),
            mediador(Lista_PID_Oxigenio, Lista_PID_Hidrogenio_Nova)
    end.

remove_elemento_lista(Posicao, Lista) ->
    Tamanho = length(Lista),
    case Lista of
        [] ->
            {error, "Lista vazia, nenhum elemento para remover"};
        _ when Posicao < 1 orelse Posicao > Tamanho ->
            {error, "Índice fora do intervalo da lista"};
        _ ->
            {Inicio, [Elemento | Fim]} = lists:split(Posicao - 1, Lista),
            {Elemento, Inicio ++ Fim}
    end.

start() ->
    Lista_PID_Hidrogenio = [],
    Lista_PID_Oxigenio = [],
    Mediador_PID = spawn(h2o, mediador, [Lista_PID_Hidrogenio, Lista_PID_Oxigenio]),
    io:format("Iniciado geração de moleculas.~n"),
    geradorDeMoleculas(Mediador_PID).