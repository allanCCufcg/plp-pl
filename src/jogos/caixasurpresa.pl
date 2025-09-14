:- module(caixasurpresa, [caixa_surpresa_menu/2, jogar_caixa_surpresa/1]).

:- use_module('../estado_global.pl').
:- encoding(utf8).
:- use_module(library(random)).
:- use_module(library(readutil)).

% ---------------- CONFIGURAÇÃO DE TEMPO ----------------
tempo_espera_abrir(2).    
tempo_espera_revelar(2).   

% ---------------- CUSTO E PRÊMIOS ----------------
custo_jogada(5).

premio_peso(0, 20).
premio_peso(5, 25).
premio_peso(10, 25).
premio_peso(25, 15).
premio_peso(50, 10).
premio_peso(75, 8).
premio_peso(100, 5).

% Expande prêmios conforme peso
expandir_premios(ListaExpandida) :-
    findall(Valor-Peso, premio_peso(Valor, Peso), VPairs),
    expand_pairs(VPairs, ListaExpandida).

expand_pairs([], []).
expand_pairs([Valor-Peso | Rest], Result) :-
    expand_value(Valor, Peso, L1),
    expand_pairs(Rest, LRest),
    append(L1, LRest, Result).

expand_value(_, N, []) :- N =< 0, !.
expand_value(Valor, N, [Valor|Rest]) :-
    N1 is N - 1,
    expand_value(Valor, N1, Rest).

% Sorteia prêmio
sorteia_premio(Premio) :-
    expandir_premios(E),
    random_member(Premio, E).

% ---------------- MENU PRINCIPAL ----------------
caixa_surpresa_menu(ID, JogarNovamente) :-
    estado_global:buscar_jogador_por_id(ID, jogador(ID, Nome, _, _, _, Saldo)),
    nl,
    writeln("==================================="),
    writeln("      🎁 Caixa-surpresa 🎁"),
    writeln("==================================="),
    format("Jogador: ~w | ID: ~w | Saldo atual: ~2f~n", [Nome, ID, Saldo]),
    writeln("-----------------------------------"),
    writeln("Regras:"),
    writeln("- Cada tentativa custa R$5."),
    writeln("- Um prêmio será sorteado com probabilidade proporcional ao peso."),
    writeln("- Possíveis prêmios: 0, 5, 10, 25, 50, 75, 100."),
    writeln("-----------------------------------"),
    write("Deseja jogar? (s/n): "), flush_output,
    read_line_to_string(user_input, Resp0),
    ( Resp0 == "" -> caixa_surpresa_menu(ID, JogarNovamente)
    ; string_lower(Resp0, Resp),
      ( sub_string(Resp, 0, 1, _, "s") ->
            jogar_caixa_surpresa(ID),
            JogarNovamente = sim
      ; sub_string(Resp, 0, 1, _, "n") ->
            JogarNovamente = nao
      ; writeln("Opção inválida."), caixa_surpresa_menu(ID, JogarNovamente)
      )
    ).

% ---------------- EXECUÇÃO DE UMA JOGADA ----------------
jogar_caixa_surpresa(ID) :-
    custo_jogada(Custo),
    estado_global:buscar_jogador_por_id(ID, jogador(ID, _, _, _, _, Saldo)),
    ( Saldo < Custo ->
        writeln("Saldo insuficiente para abrir a caixa-surpresa (custo: R$5).")
    ;   writeln("Abrindo a caixa..."),
        tempo_espera_abrir(T1), sleep(T1),

        writeln("✨ Revelando prêmio..."),
        tempo_espera_revelar(T2), sleep(T2),

        sorteia_premio(Premio),
        format(">>> Prêmio obtido: R$~w~n", [Premio]),

        % Espera extra só para o jogador ver o prêmio
        sleep(2),

        estado_global:registrar_jogada(ID, "caixa_surpresa", Custo, Premio),
        format("Saldo atualizado (após aposta de R$~w e ganho de R$~w).~n", [Custo, Premio])
    ).
