:- module(blackjack, [blackjack_menu/2, jogar_blackjack/1]).

:- use_module('../estado_global.pl').
:- encoding(utf8).
:- use_module(library(random)).
:- use_module(library(readutil)).

% ===== Definição de cartas =====
naipe(espadas). naipe(copas). naipe(ouros). naipe(paus).
valor(as). valor(2). valor(3). valor(4). valor(5). valor(6). valor(7). valor(8). valor(9). valor(10). valor(valete). valor(dama). valor(rei).

suit_symbol(espadas, "♠"). suit_symbol(copas, "♥"). suit_symbol(ouros, "♦"). suit_symbol(paus, "♣").
value_str(as, "A"). value_str(valete, "J"). value_str(dama, "Q"). value_str(rei, "K").
value_str(N, S) :- number(N), number_string(N, S).

carta(V, N) :- valor(V), naipe(N).

% ===== Baralho =====
baralho(Deck) :- findall(carta(V,N), (valor(V), naipe(N)), Deck).

% ===== Mostrar cartas =====
show_card(carta(V,N), Str) :- value_str(V, Vs), suit_symbol(N, Ss), string_concat(Vs, Ss, Str).
mostrar_mao(Mao, Str) :- maplist(show_card, Mao, ListaStr), atomic_list_concat(ListaStr, " ", Atom), atom_string(Atom, Str).

make_placeholders(N, PlaceStr) :- N >= 0, length(L, N), maplist(=("??"), L), atomic_list_concat(L, ' ', Atom), atom_string(Atom, PlaceStr).
show_dealer_hidden([First|Rest], Str) :- 
    show_card(First, FirstStr), 
    length(Rest, N), 
    (N =:= 0 -> Str = FirstStr ; make_placeholders(N, PlaceStr), string_concat(FirstStr, " ", Temp), string_concat(Temp, PlaceStr, Str)).

% ===== Valores =====
valor_carta(carta(as,_), 1). 
valor_carta(carta(V,_), V) :- number(V). 
valor_carta(carta(valete,_), 10). 
valor_carta(carta(dama,_), 10). 
valor_carta(carta(rei,_), 10).

pontuacao(Mao, Pontos) :- 
    maplist(valor_carta, Mao, Valores), 
    sum_list(Valores, Base), 
    include(==(1), Valores, Ases), 
    length(Ases, NumAs), 
    ajustar_as(Base, NumAs, Pontos).

ajustar_as(Total, 0, Total). 
ajustar_as(Total, N, Final) :- 
    (Total + 10 =< 21 -> T1 is Total + 10, N1 is N - 1, ajustar_as(T1, N1, Final) ; Final = Total).

% ===== Baralho e cartas =====
shuffle_deck(Deck, Shuffled) :- random_permutation(Deck, Shuffled).
draw_card([C|Rest], C, Rest). 
draw_card([], _, _) :- fail.

% ===== Leitura de escolha do jogador =====
read_choice(Choice) :-
    write("Digite 'h' para hit (puxar) ou 's' para stand (parar): "),
    flush_output,
    read_line_to_string(user_input, Raw),
    ( Raw == "" -> read_choice(Choice)
    ; string_lower(Raw, L),
      ( sub_string(L, 0, 1, _, "h") -> Choice = hit
      ; sub_string(L, 0, 1, _, "s") -> Choice = stand
      ; writeln("Escolha inválida."), read_choice(Choice)
      )
    ).

% ===== Menu principal do Blackjack =====
blackjack_menu(ID, JogarNovamente) :-
    estado_global:buscar_jogador_por_id(ID, jogador(ID, Nome, _, _, _, Saldo)),
    nl,
    writeln("==================================="),
    writeln("      🃏 Blackjack 🃏"),
    writeln("==================================="),
    format("Jogador: ~w | ID: ~w | Saldo atual: ~d~n", [Nome, ID, Saldo]),
    writeln("==================================="),
    writeln("Regras do Blackjack:"),
    writeln("- Objetivo: chegar o mais próximo possível de 21 sem estourar."),
    writeln("- Cada rodada custa 10 unidades de saldo."),
    writeln("- Vitória: ganha 20 unidades; empate: recebe de volta 10; derrota: perde a aposta."),
    writeln("- Ases valem 1 ou 11, figuras valem 10."),
    writeln("- Se o dealer fizer 21 nas duas primeiras cartas, ele vence automaticamente."),
    writeln("-----------------------------------"),
    write("Deseja jogar? (s/n): "), flush_output,
    read_line_to_string(user_input, Resp),
    string_lower(Resp, LResp),
    ( sub_string(LResp,0,1,_, "s") -> jogar_blackjack(ID), JogarNovamente = sim
    ; sub_string(LResp,0,1,_, "n") -> JogarNovamente = nao
    ; writeln("Opção inválida."), blackjack_menu(ID, JogarNovamente)
    ).

% ===== Loop principal do jogo =====
game_loop(Deck, Player, Dealer, StandP, StandD, ID, Saldo, SaldoFinal) :-
    pontuacao(Player, Pp),
    mostrar_mao(Player, PlayerStr),
    show_dealer_hidden(Dealer, DealerHidden),
    nl,
    format("Sua mão: ~s (Pontos: ~d)~n", [PlayerStr, Pp]),
    format("Mão do dealer: ~s~n", [DealerHidden]),

    % Jogador decide
    ( Pp > 21 -> NewPlayer = Player, NewDeckP = Deck, StandPNext = true
    ; StandP -> NewPlayer = Player, NewDeckP = Deck, StandPNext = true
    ; read_choice(Choice),
      ( Choice == hit -> draw_card(Deck, CardP, Deck1), append(Player, [CardP], NewPlayer), NewDeckP = Deck1, StandPNext = false
      ; Choice == stand -> NewPlayer = Player, NewDeckP = Deck, StandPNext = true
      )
    ),

    % Dealer joga
    pontuacao(Dealer, PdNow),
    ( StandD -> NewDealer = Dealer, NewDeck = NewDeckP, StandDNext = true
    ; PdNow < 17 -> draw_card(NewDeckP, CardD, Deck2), append(Dealer, [CardD], NewDealer), NewDeck = Deck2, StandDNext = false
    ; NewDealer = Dealer, NewDeck = NewDeckP, StandDNext = true
    ),

    % Verifica fim do jogo
    ( StandPNext, StandDNext ->
        pontuacao(NewPlayer, PFinal),
        pontuacao(NewDealer, DFinal),
        mostrar_mao(NewDealer, DealerStrFinal),
        format("Mão final do dealer: ~s (Pontos: ~d)~n", [DealerStrFinal, DFinal]),
        mostrar_mao(NewPlayer, PlayerStrFinal),
        format("Sua mão final: ~s (Pontos: ~d)~n", [PlayerStrFinal, PFinal]), nl,

        % ===== Decisão do vencedor =====
        ( PFinal =< 21, DFinal =< 21 ->
            ( PFinal > DFinal -> writeln("Você venceu!"), Ganho = 20
            ; DFinal > PFinal -> writeln("Dealer venceu!"), Ganho = 0
            ; writeln("Empate!"), Ganho = 10
            )
        ; PFinal > 21, DFinal =< 21 -> writeln("Dealer venceu!"), Ganho = 0
        ; DFinal > 21, PFinal =< 21 -> writeln("Você venceu!"), Ganho = 20
        ; PFinal > 21, DFinal > 21 ->
            % Ambos estouraram: quem ultrapassou menos ganha
            DiferencaP is PFinal - 21,
            DiferencaD is DFinal - 21,
            ( DiferencaP < DiferencaD -> writeln("Você venceu! (menos estourado)"), Ganho = 20
            ; DiferencaD < DiferencaP -> writeln("Dealer venceu! (menos estourado)"), Ganho = 0
            ; writeln("Empate! (ambos estouraram)"), Ganho = 10
            )
        ),

        % Registra jogada e atualiza saldo
        estado_global:registrar_jogada(ID, "blackjack", 10, Ganho),
        SaldoFinal is Saldo - 10 + Ganho

    ; game_loop(NewDeck, NewPlayer, NewDealer, StandPNext, StandDNext, ID, Saldo, SaldoFinal)
    ).

% ===== Jogar uma rodada =====
jogar_blackjack(ID) :-
    estado_global:buscar_jogador_por_id(ID, jogador(ID,_,_,_,_,SaldoAtual)),
    ( SaldoAtual < 10 -> writeln("Saldo insuficiente para jogar!")
    ; Saldo is SaldoAtual - 10,
      baralho(B0), shuffle_deck(B0, B1),
      draw_card(B1, C1, B2), draw_card(B2, C2, B3), Player = [C1, C2],
      draw_card(B3, D1, B4), draw_card(B4, D2, B5), Dealer = [D1, D2],

      pontuacao(Dealer, Pd),
      ( Pd =:= 21 -> mostrar_mao(Dealer, DealerStr), format("Dealer fez Blackjack! Mão: ~s~n", [DealerStr]), writeln("Dealer venceu!")
      ; game_loop(B5, Player, Dealer, false, false, ID, Saldo, _)
      )
    ).
