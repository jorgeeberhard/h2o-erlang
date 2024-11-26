-module(h2o).
-export([start/0, oxigenio/1, hidrogenio/1, geradorDeMoleculas/1, mediador/2]).

% Gerá um processo para cada molecula aleatóriamente
geradorDeMoleculas(Mediador_PID) ->
    timer:sleep(round(timer:seconds(10))),
    X = rand:uniform(10),
    if
        X >= 6 -> 
            spawn(h2o, oxigenio, [Mediador_PID]); % Processo para Oxigenio, recebe PID do mediador
        X =< 5 ->
            spawn(h2o, hidrogenio, [Mediador_PID]) % Processo para Hidrogênio, recebe PID do mediador
    end,
    geradorDeMoleculas(Mediador_PID).

hidrogenio(Mediador_PID) ->
    Hidrogenio_PID = self(),
    io:format("Gerado molecula número ~p de Hidrogênio~n", [Hidrogenio_PID]), % Informa e identifica a molecula gerada
    timer:sleep(round(timer:seconds(rand:uniform(20)+10))), % Garante que o tempo para ter energia esteja entre 10 e 30 segundos
    Mediador_PID ! {hidrogenio, self()}, % Chama o mediador para informar que está com energia suficiente
    receive
        kill ->
            exit(shutdown)
    end.


oxigenio(Mediador_PID) ->
    Oxigenio_PID = self(),
    io:format("Gerado molecula número ~p de Oxigênio~n", [Oxigenio_PID]), % Informa e identifica a molecula gerada
    timer:sleep(round(timer:seconds(rand:uniform(20)+10))), % Garante que o tempo para ter energia esteja entre 10 e 30 segundos
    Mediador_PID ! {oxigenio, self()}, % Chama o mediador para informar que está com energia suficiente
    receive
        kill ->
            exit(shutdown)
    end.
    
mediador(Lista_PID_Oxigenio, Lista_PID_Hidrogenio) ->
    if 
        length(Lista_PID_Hidrogenio) >= 2 andalso length(Lista_PID_Oxigenio) >= 1 -> % Verifica se a lista de Hidrogênio tenha pelo menos 2, e se a lista de Oxigênio tenha pelo menos 1
            {Hidrogenio_PID_1, Lista_PID_Hidrogenio_1} = remove_elemento_lista(1, Lista_PID_Hidrogenio), 
            {Hidrogenio_PID_2, Lista_PID_Hidrogenio_2} = remove_elemento_lista(1, Lista_PID_Hidrogenio_1),
            {Oxigenio_PID_1, Lista_PID_Oxigenio_1} = remove_elemento_lista(1, Lista_PID_Oxigenio),
            io:format("Formado molecula H2O com elementos: (~p + ~p + ~p).~n", [Hidrogenio_PID_1, Hidrogenio_PID_2, Oxigenio_PID_1]), % Informa quais as moleculas que se combinaram
            Hidrogenio_PID_1 ! kill, % Termina o processo de um dos Hidrogênios
            Hidrogenio_PID_2 ! kill, % Termina o processo do segundo Hidrogênio
            Oxigenio_PID_1 ! kill, % Termina o processo de um dos Oxigênios
            mediador(Lista_PID_Oxigenio_1, Lista_PID_Hidrogenio_2); % Recursividade com a própria função, passando novos valores de listas
        true ->
            io:format("")
    end,
    receive % Aguarda uma molecula de Hidrogênio ou Oxigênio informar que tem energia suficiente
        {oxigenio, Oxigenio_PID} ->
            io:format("Molecula de Oxigênio número ~p está com energia suficiente~n", [Oxigenio_PID]),
            Lista_PID_Oxigenio_Nova = [ Oxigenio_PID | Lista_PID_Oxigenio ], % Adiciona a molecula na lista de Oxigênios COM energia
            mediador(Lista_PID_Oxigenio_Nova, Lista_PID_Hidrogenio); % Chama a função recursivamente com novos valores de lista
        {hidrogenio, Hidrogenio_PID} ->
            io:format("Molecula de Hidrogênio número ~p está com energia suficiente!~n", [Hidrogenio_PID]),
            Lista_PID_Hidrogenio_Nova = [ Hidrogenio_PID | Lista_PID_Hidrogenio ], % Adiciona a molecula na lista de Hidrogênios COM energia
            mediador(Lista_PID_Oxigenio, Lista_PID_Hidrogenio_Nova) % Chama a função recursivamente com novos valores de lista
    end.

% Função para remover elemento da lista, de acordo com posição
remove_elemento_lista(Posicao, Lista) ->
    Tamanho = length(Lista),
    case Lista of
        [] ->
            {error, "Lista vazia, nenhum elemento para remover"};
        _ when Posicao < 1 orelse Posicao > Tamanho ->
            {error, "Índice fora do intervalo da lista"};
        _ ->
            {Inicio, [Elemento | Fim]} = lists:split(Posicao - 1, Lista),
            {Elemento, Inicio ++ Fim} % Retorna o elemento removido, e a nova lista
    end.

start() ->
    Lista_PID_Hidrogenio = [], % Inicia lista vazia de PID de Hidrogênio
    Lista_PID_Oxigenio = [], % Inicia lista vazia de PID de Oxigênio
    Mediador_PID = spawn(h2o, mediador, [Lista_PID_Hidrogenio, Lista_PID_Oxigenio]), % Inicia processo do mediador
    io:format("Iniciado geração de moleculas.~n"),
    geradorDeMoleculas(Mediador_PID). % Inicia o gerador de moleculas, informando o PID do Mediador.