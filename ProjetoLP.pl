/* Projeto LP - Pedro Noronha - 102543 - LEIC_T */




/* Predicado extrai_ilhas_linha(Numero_de_linha, Linha, Ilhas) */
extrai_ilhas_linha(N_L, Linha, Ilhas) :- extrai_ilhas_linha(N_L, Linha, Ilhas, 0, []).
% quando acaba a iteracao sobre a lista Linha unificacao com [] (lista vazia) e Ilha unifica com N_ilha (Acc, Acc).
extrai_ilhas_linha(_, [], Acc, _, Acc).
% iteracao sobre a lista Linha, quando Head da lista =/= 0 existe uma ilha.
extrai_ilhas_linha(N_L, [H|T], Ilhas, N, N_ilha) :- N1 is N + 1,
    H == 0, !,
    extrai_ilhas_linha(N_L, T, Ilhas, N1, N_ilha).
extrai_ilhas_linha(N_L, [H|T], Ilhas, N, N_ilha) :- N1 is N + 1,
    H \== 0, !,
    % dado append da lista N_ilha com a nova ilha na lista N_ilhas, volta se a chamar o predicado 
    % com N_ilhas (lista de acumulacao) que vai unificar no final com Ilhas (todas as ilhas da Linha)
    append(N_ilha, [ilha(H, (N_L, N1))], N_ilhas),
    extrai_ilhas_linha(N_L, T, Ilhas, N1, N_ilhas).


/* Predicado ilhas(Puzzle, Ilhas_do_puzzle) */
% iteracao sobre os elementos do puzzle (linhas) em cada linha chama se o predicado anterior
% extrai_ilhas_linhas dando append a cada iteracao das novas ilhas de cada linha na lista N_Ilhas1,
% que vai unificar no final da iteracao com a lista Ilhas.
ilhas(Puz, Ilhas) :- ilhas(Puz, Ilhas, 0, []).
ilhas([], Acc, _, Acc).
ilhas([H|T], Ilhas, N, N_Ilhas) :- N1 is N + 1,
    extrai_ilhas_linha(N1, H, Ilhas_linha),
    append(N_Ilhas, Ilhas_linha, N_Ilhas1),
    ilhas(T, Ilhas, N1, N_Ilhas1).


/* Predicado auxiliar obter_pos(Ilha, Pos), facilita obter a posicao de uma ilha nos predicados principais */
obter_pos(ilha(_, Pos), (Pos)).

/* Predicado auxiliar obter_x(Pos, CoordenadaX), facilita obter a coordenada X de uma posicao */
obter_x((X, _), X).

/* Predicado auxiliar obter_y(Pos, CoordenadaY), facilita obter a coordenada Y de uma posicao */
obter_y((_, Y), Y).

/* Predicado list_null([]), representa lista vazia para utilizacao de exclude de listas vizinhas
nos predicados principais */
list_null([]).


/* Predicado vizinhas(Ilhas_do_puzzle, Ilha, Vizinhas_da_ilha), devolve as lista de ilhas do puzzle
diretamente adjacentes a Ilha */
vizinhas(Ilhas, Ilha, Vizinhas) :- obter_pos(Ilha, Pos), obter_x(Pos, X), obter_y(Pos, Y),
    % depois de obtidas as coordenadas da Ilha, 
    findall(ilha(P, (X1, Y1)), (member(ilha(P, (X1, Y1)), Ilhas), (X1 == X ; Y1 == Y)), L_Viz),
    % utiliza se findall para encontrar todas as ilhas na mesma coluna ou linha da Ilha
    % Quatro findall diferentes:
    % Ilhas na mesma linha mas a baixo da Ilha:
    findall(ilha(P, (X1, Y1)), (member(ilha(P, (X1, Y1)), L_Viz), Y1 > Y), L_Y_maior),
    % Ilhas na mesma linha mas a cima da Ilha:
    findall(ilha(P, (X1, Y1)), (member(ilha(P, (X1, Y1)), L_Viz), Y1 < Y), L_Y_menor),
    % Ilhas na mesma coluna mas a direita da Ilha:
    findall(ilha(P, (X1, Y1)), (member(ilha(P, (X1, Y1)), L_Viz), X1 > X), L_X_maior),
    % Ilhas na mesma coluna mas a esquerda da Ilha:
    findall(ilha(P, (X1, Y1)), (member(ilha(P, (X1, Y1)), L_Viz), X1 < X), L_X_menor),
    % Com as quatros linhas criadas, verifica-se se sao vazias ou nao, se nao forem o primeiro elemento
    % das listas L_Y_maior e L_X_maior corresponde a ilha vizinhas, nas listaS L_Y_menor e L_X_menor
    % o ultimo elemento e a ilha vizinha por isso faz se 'reverse' da lista e retira se o primeiro elemento.
    % Se uma das listas for vazia a ilha vizinha nessa lista sera [].
    (L_Y_maior == [] -> Y_maior = []; L_Y_maior = [Y_maior|_]), 
    (L_X_maior == [] -> X_maior = []; L_X_maior = [X_maior|_]),
    (L_Y_menor == [] -> Y_menor = []; reverse(L_Y_menor, L_y_menor), L_y_menor = [Y_menor|_]),
    (L_X_menor == [] -> X_menor = []; reverse(L_X_menor, L_x_menor), L_x_menor = [X_menor|_]),
    append([], [X_menor, Y_menor, Y_maior, X_maior], Vizinhas1),
    % retira se as listas vazias da provisoria lista Vizinhas1 para formar a lista final Vizinhas.
    exclude(list_null, Vizinhas1, Vizinhas).



/* Predicado estado(Ilhas_do_Puzzle, Estado_do_Puzzle), atraves da lista de ilhas do puzzle,
   sao retiradas as entradas do Puzzle (cada entrada representa informacao relativa a ilha,
   (ilha, vizinhas da ilha, pontes da ilha). */

% Iteracao sobre a lista Ilhas.
estado(Ilhas, Estado) :- estado(Ilhas, Estado, Ilhas, []).
estado(_, Est, [], Est).
estado(Ilhas, Estado, Ilhas_N, Estado_N) :- Ilhas_N = [H|T],
    % E chamado o predicado vizinhas para cada ilha da lista,
    vizinhas(Ilhas, H, Vizinhas),
    % E criada a nova entrada com a ilha, vizinhas da ilha e as pontes (por agora lista vazia)
    append([], [H, Vizinhas, []], Entrada),
    % A cada entrada criada e acumulada ao Estado_N que no final da iteracao unifica com Estado
    append(Estado_N, [Entrada], Estado_N1),
    estado(Ilhas, Estado, T, Estado_N1).
    


/* Predicado posicoes_entre(Pos1, Pos2, Posicoes_entre_Pos1_Pos2), obtem todas as posicoes entre
   duas posicoes que estao na mesma linha ou coluna. */
posicoes_entre(Pos1, Pos2, Posicoes) :- obter_x(Pos1, X1), obter_x(Pos2, X2),
    obter_y(Pos1, Y1), obter_y(Pos2, Y2),
    % depois de obter as coordenadas das duas posicoes, averigua-se se estao na mesma linha ou coluna.
    X1 == X2,
    % utiliza-se o predicado n_entre(N1, N2, Numeros_entre) que devolve todos os numeros entre dois numeros,
    % com N1 < N2.
    (Y2 > Y1 -> n_entre(Y1, Y2, N);
    n_entre(Y2, Y1, N)),
    % utiliza-se o predicado auxiliar obter_posicoes(Coordenada, Coordenada_fixa, Lista_de_Numeros_entre, 
    % Posicoes) que devolve todas as posicoes formadas pela coordenada fixa e pela lista de numeros.
    % Neste caso o primeiro argumento de obter_posicoes sera o numero 1 para indicar que a coordenada fixa
    % esta na posicao X
    obter_posicoes(1, X1, N, Posicoes).
posicoes_entre(Pos1, Pos2, Posicoes) :- obter_x(Pos1, X1), obter_x(Pos2, X2),
    % o processo e semelhante ao de cima mas para quando a coordenada fixa esta na posicao Y, o 
    % primeiro argumento do obter_posicoes sera um 0.
    obter_y(Pos1, Y1), obter_y(Pos2, Y2),
    Y1 == Y2,
    (X2 > X1 -> n_entre(X1, X2, N);
    n_entre(X2, X1, N)),
    obter_posicoes(0, Y1, N, Posicoes).



/* Predicado auxiliar n_entre(N1, N2, Numeros_entre_N1_N2), o predicado devolve todos os numeros inteiros
   entre N1 e N2 com N1 < N2. */
n_entre(N1, N2, NL) :- N_aux is N1 + 1, n_entre(N1, N2, NL, N_aux, []).
% quando o N_aux chega a N2 o predicado para e NL_aux(lista de todos os numeros de N1 a N2) unifica com NL.
n_entre(_, N2, NL_aux, N2, NL_aux).
n_entre(N1, N2, NL, N_aux, NL_aux) :- N_aux < N2,
    append(NL_aux, [N_aux], NL_aux1),
    N_aux1 is N_aux + 1,
    n_entre(N1, N2, NL, N_aux1, NL_aux1).


/* Predicado auxiliar obter_posicoes(Coordenada_XouY, Coordenada_fixa, Lista_numeros, Posicoes),
   se Coord == 1 a coordenada fixa sera X se for == 0 entao sera Y */
obter_posicoes(Coord, X, N, Posicoes) :- obter_posicoes(Coord, X, N, Posicoes, N, []).
obter_posicoes(_, _, _, Pos_aux, [], Pos_aux).
obter_posicoes(Coord, X, N, Posicoes, N_aux, Posicoes_aux) :- N_aux = [H|T],
    Coord = 1,
    Pos = (X, H),
    append(Posicoes_aux, [Pos], Posicoes_aux1),
    obter_posicoes(Coord, X, N, Posicoes, T, Posicoes_aux1).
obter_posicoes(Coord, X, N, Posicoes, N_aux, Posicoes_aux) :- N_aux = [H|T],
    Coord = 0,
    Pos = (H, X),
    append(Posicoes_aux, [Pos], Posicoes_aux1),
    obter_posicoes(Coord, X, N, Posicoes, T, Posicoes_aux1).


/* Predicado cria_ponte(Pos1, Pos2, Ponte), predicado cria uma ponte entre as duas posicoes. */
cria_ponte(Pos1, Pos2, Ponte) :- Pos1 = (X1, Y1),
    % quando a pos1 esta a esquerda ou a cima da pos2 -> ponte(pos1, pos2)
    Pos2 = (X2, Y2),
    (Y1 == Y2, X1 =< X2;
    X1 == X2, Y1 =< Y2),
    Ponte = ponte(Pos1, Pos2).

cria_ponte(Pos1, Pos2, Ponte) :- Pos1 = (X1, Y1),
    % quando a pos2 esta a esquerda ou a cima da pos1 -> ponte(pos2, pos1)
    Pos2 = (X2, Y2),
    (Y1 == Y2, X1 >= X2;
    X1 == X2, Y1 >= Y2),
    Ponte = ponte(Pos2, Pos1).



/* Predicado caminho_livre(Posicao1. Posicao2, Posicoes, Ilha, Vizinha_da_ilha), 
   devolve true se o caminho entre Posicao1 e Posicao2 estiver livre e false se nao estiver. */
caminho_livre(Pos1, Pos2, Posicoes, I, Vz) :- obter_pos(I, PosI), obter_pos(Vz, PosVz),
    % atraves do predicado posicoes_entre obtemos a lista de posicoes entre Pos da ilha e a Pos da vizinha
    posicoes_entre(PosI, PosVz, Posicoes1),
    % junta se Pos1 e Pos2 a lista de posicoes entre Pos1 e Pos2
    append(Posicoes, [Pos1, Pos2], Posicoes2),
    % encontra se todos os elementos comuns a lista de posicoes entre Ilha e Vizinha e os elementos
    % da lista de posicoes entre Pos1 e Pos2.
    findall(E, (member(E, Posicoes2), member(E, Posicoes1)), Pos_comum),
    % N sera o numero de elementos comuns as duas listas.
    length(Pos_comum, N),
    % Ha tres casos em que o caminho sera livre, quando N == 0 ou quando Pos1 e Pos2 sao iguais a PosIlha
    % e PosVizinha.
    (N == 0; (Pos1 == PosVz, Pos2 == PosI); (Pos2 == PosVz, Pos1 == PosI)).



/* Predicado actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes_entre_Pos1Pos2, Entrada_puzzle, Nova_Entrada),
   verifica se o caminho entre a ilha da Entrada e cada uma das suas vizinhas esta livre, se nao estiver
   essa vizinha e retirada da lista de Vizinhas na entrada. */
actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, Entrada, Nova_Entrada) :- nth1(2, Entrada, Vizinhas),
    nth1(1, Entrada, Ilha),
    % atraves do findall verificamos todas as vizinhas da ilha se o caminho esta livre entre esta mesma e a ilha.
    findall(Viz, (member(Viz, Vizinhas), caminho_livre(Pos1, Pos2, Posicoes, Ilha, Viz)), Vizinhas_atualizada),
    nth1(3, Entrada, Ponte),
    Nova_Entrada = [Ilha, Vizinhas_atualizada, Ponte].


/* Predicado actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, Novo_Estado), a cada entrada do estado e
   aplicado o predicado actualiza_vizinhas_entrada e devolve um Novo_Estado com as entradas atualizadas. */
actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, Novo_Estado) :- 
    actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, Novo_Estado, Estado, []).
actualiza_vizinhas_apos_pontes(_, _, _, Novo_Estado_final, [], Novo_Estado_final).
actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, Novo_Estado, [H|T], Novo_Estado_Aux) :- 
    posicoes_entre(Pos1, Pos2, Posicoes),
    actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, H, NovaEntrada),
    append(Novo_Estado_Aux, [NovaEntrada], Novo_Estado_Aux1),
    actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, Novo_Estado, T, Novo_Estado_Aux1).



/* Predicado auxiliar obter_n_pontes(ilha, numero de pontes de ligacao da ilha) extrai o numero 
   de pontes necessarias de uma ilha. */
obter_n_pontes(ilha(N_pontes, _), N_pontes).


/* Predicado ilhas_terminadas(Estado, Ilhas_terminadas), devolve todas as ilhas terminadas,
   (com todas as pontes necessarias) na lista Ilhas_term. */
ilhas_terminadas(Estado, Ilhas_term) :- ilhas_terminadas(Estado, Ilhas_term, []).
ilhas_terminadas([], Ilhas_term_final, Ilhas_term_final).
% iteramos sobre todas as entradas do estado.
ilhas_terminadas([H|T], Ilhas_term, Ilhas_term_aux) :- nth1(1, H, Ilha),
    obter_n_pontes(Ilha, N_pontes), nth1(3, H, Pontes),
    length(Pontes, N),
    % para cada ilha de cada entrada do estado obtemos o numeros de pontes que tem e o numero necessario
    % para a ilha estar terminada.
    % Se os dois numeros forem iguais a ilha e uma ilha terminada e adiciona se a ilha a lista Ilhas_term.
    ((N == N_pontes, N_pontes \== 'X') -> append(Ilhas_term_aux, [Ilha], Ilhas_term_aux1),
    ilhas_terminadas(T, Ilhas_term, Ilhas_term_aux1)); 
    ilhas_terminadas(T, Ilhas_term, Ilhas_term_aux).



/* Predicado tira_ilhas_terminadas_entrada(Ilhas_terminadas, Entrada, Nova_Entrada), recebe
   uma lista de ilhas terminadas e uma Entrada e retira as ilhas terminadas da Entrada. */
tira_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_Entrada) :- nth1(2, Entrada, Vizinhas),
    % atraves do findall econtram se todas as ilhas que sao vizinhas da Ilha da entrada e que
    % nao sao ilhas terminadas, assim obtemos com esses elementos a lista Nova_Vizinhas.
    findall(Ilha, (member(Ilha, Vizinhas), \+ member(Ilha, Ilhas_term)), Nova_Vizinhas),
    nth1(1, Entrada, Ilha), nth1(3, Entrada, Pontes),
    % A nova entrada e composta pela ilha da entrada inicial, a lista Nova_Vizinhas e as Pontes originais.
    Nova_Entrada = [Ilha, Nova_Vizinhas, Pontes].


/* Predicado tira_ilhas_terminadas(Estado, Ilhas_terminadas, Novo_Estado), aplica se o predicado
   tira_ilhas_terminadas_entrada a cada entrada do Estado, o Novo_Estado e composto pelas Novas_entradas */
tira_ilhas_terminadas(Estado, Ilhas_term, Novo_Estado) :- 
    tira_ilhas_terminadas(Estado, Ilhas_term, Novo_Estado, []).
tira_ilhas_terminadas([], _, Novo_Estado_final, Novo_Estado_final).
tira_ilhas_terminadas([H|T], Ilhas_term, Novo_Estado, Novo_Estado_Aux) :- 
    tira_ilhas_terminadas_entrada(Ilhas_term, H, Nova_Entrada),
    append(Novo_Estado_Aux, [Nova_Entrada], Novo_Estado_Aux1),
    tira_ilhas_terminadas(T, Ilhas_term, Novo_Estado, Novo_Estado_Aux1).


/* Predicado marca_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_Entrada) marca a
   ilha da entrada com um 'X' no lugar do numero de pontes para estar terminada se a ilha
   estiver terminada. */
marca_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_Entrada) :- nth1(1, Entrada, Ilha),
    % verifica se a ilha da entrada esta na lista Ilhas_terminadas.
    member(Ilha, Ilhas_term),
    obter_pos(Ilha, Pos),
    Ilha_atualizada = ilha('X', Pos),
    nth1(2, Entrada, Vizinhas),
    nth1(3, Entrada, Pontes),
    % cria a nova entrada com a ilha atualizada.
    Nova_Entrada = [Ilha_atualizada, Vizinhas, Pontes].
marca_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_Entrada) :- nth1(1, Entrada, Ilha),
    \+member(Ilha, Ilhas_term),
    Nova_Entrada = Entrada.



/* Predicado marca_ilhas_terminadas(Estado, Ilhas_term, Novo_Estado), aplica o predicado
   marca_ilhas_terminadas_entrada a cada entrada do Estado, o Novo_Estado e composto pelas Novas_Entradas. */
marca_ilhas_terminadas(Estado, Ilhas_term, Novo_Estado) :- 
    marca_ilhas_terminadas(Estado, Ilhas_term, Novo_Estado, []).
marca_ilhas_terminadas([], _, Novo_Estado_final, Novo_Estado_final).
marca_ilhas_terminadas([H|T], Ilhas_term, Novo_Estado, Novo_Estado_Aux) :- 
    marca_ilhas_terminadas_entrada(Ilhas_term, H, Nova_Entrada),
    append(Novo_Estado_Aux, [Nova_Entrada], Novo_Estado_Aux1),
    marca_ilhas_terminadas(T, Ilhas_term, Novo_Estado, Novo_Estado_Aux1).



/* Predicado trata_ilhas_terminadas(Estado, Novo_Estado), aplica os predicados ilhas_terminadas ao Estado
   para se obter a lista de ilhas terminadas no Estado, depois sao aplicados os predicados tira_ilhas_terminadas,
   e marca_ilhas_terminadas ao Estado, obtendo o Novo_Estado. */
trata_ilhas_terminadas(Estado, Novo_Estado) :- ilhas_terminadas(Estado, Ilhas_term),
    tira_ilhas_terminadas(Estado, Ilhas_term, Novo_Estado_Aux),
    marca_ilhas_terminadas(Novo_Estado_Aux, Ilhas_term, Novo_Estado).


/* Predicado auxiliar obter_pos_ponte1(Ponte, Posicao1) que auxilia nos predicados principais a obter
   a primeira posicao de uma ponte */
obter_pos_ponte1(ponte(Pos1, _), Pos1).


/* Predicado auxiliar obter_pos_ponte2(Ponte, Posicao2) que auxilia nos predicados principais a obter
   a segunda posicao de uma ponte */
obter_pos_ponte2(ponte(_, Pos2), Pos2).


/* Predicado auxiliar junta_pondes_aux(Estado, Num_Pontes, Ponte, Novo_Estado), */
junta_pontes_aux(Estado, Num_Pontes, Ponte, Novo_Estado) :- 
    junta_pontes_aux(Estado, Num_Pontes, Ponte, Novo_Estado, []).
junta_pontes_aux([], _, _, Estado_final, Estado_final).
% vamos iterar por cada entrada do estado.
junta_pontes_aux([H|T], Num_Pontes, Ponte, Novo_Estado, Novo_Estado_Aux) :- nth1(1, H, Ilha),
    obter_pos(Ilha, Pos_I), obter_pos_ponte1(Ponte, Pos_P1), obter_pos_ponte2(Ponte, Pos_P2),
    % e criada uma lista com as duas posicoes das pontes.
    Pos_Ponte = [Pos_P1, Pos_P2], 
    % encontrar as ilhas entre as quais vai ser criada a ponte.
    member(Pos_I, Pos_Ponte), !,
    nth1(2, H, Vizinhas), nth1(3, H, Pontes),
    % atraves do maplist e colocada a ponte em cada ilha em que a ponte e criada.
    length(Nova_Pontes_Aux, Num_Pontes), maplist(=(Ponte), Nova_Pontes_Aux), 
    % a nova entrada introduzida no novo estado contem a ilha atualizada.
    append(Pontes, Nova_Pontes_Aux, Nova_Pontes), Nova_Entrada = [Ilha, Vizinhas, Nova_Pontes], 
    append(Novo_Estado_Aux, [Nova_Entrada], Novo_Estado_Aux1),
    junta_pontes_aux(T, Num_Pontes, Ponte, Novo_Estado, Novo_Estado_Aux1).
junta_pontes_aux([H|T], Num_Pontes, Ponte, Novo_Estado, Novo_Estado_Aux) :- 
    append(Novo_Estado_Aux, [H], Novo_Estado_Aux1),
    junta_pontes_aux(T, Num_Pontes, Ponte, Novo_Estado, Novo_Estado_Aux1).



/* Predicado junta_pontes(Estado, Numero_pontes, Ilha1, Ilha2, Novo_Estado), devolve um novo estado,
   com a ponte entre Ilha1 e Ilha2, as ilhas terminadas que a existencia dessa nova ponte podem gerar
   tratadas atraves do predicado trata_ilhas_ponte e as vizinhas actualizadas depois da ponte. */
junta_pontes(Estado, Num_Pontes, Ilha1, Ilha2, Novo_Estado) :- obter_pos(Ilha1, Pos1), obter_pos(Ilha2, Pos2),
    cria_ponte(Pos1, Pos2, Ponte),
    junta_pontes_aux(Estado, Num_Pontes, Ponte, Novo_Estado1),
    actualiza_vizinhas_apos_pontes(Novo_Estado1, Pos1, Pos2, Novo_Estado2),
    trata_ilhas_terminadas(Novo_Estado2, Novo_Estado).




