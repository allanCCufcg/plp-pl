:- set_prolog_flag(encoding, utf8).

:- use_module('estado_global.pl').
:- use_module('jogos/blackjack.pl').
:- use_module(library(readutil)).

% --------- Menu principal ----------
main :-
    estado_global:inicializar_sistema,
    menu_principal.

menu_principal :-
    nl, writeln("==================================="),
    writeln("   🎰 Bem-vindo ao Cassino Prolog 🎰"),
    writeln("==================================="),
    writeln("1 - Login"),
    writeln("2 - Criar conta"),
    writeln("3 - Listar jogadores"),
    writeln("4 - Mostrar ranking"),
    writeln("0 - Sair"),
    write("Escolha: "), flush_output,
    read_line_to_string(user_input, Input),
    handle_main_menu(Input).

handle_main_menu("1") :- login.
handle_main_menu("2") :- criar_conta.
handle_main_menu("3") :- estado_global:listar_jogadores, menu_principal.
handle_main_menu("4") :- estado_global:mostrar_ranking, menu_principal.
handle_main_menu("0") :- writeln("Saindo...").
handle_main_menu(_) :- writeln("Opção inválida."), menu_principal.

% --------- Login ----------
login :-
    write("Digite seu ID: "), flush_output,
    read_line_to_string(user_input, Input),
    number_string(ID, Input),
    ( estado_global:buscar_jogador_por_id(ID, Jogador),
      Jogador \= none
    -> Jogador = jogador(ID, Nome, _, _, _, Saldo),
       format("Bem-vindo, ~w!~n", [Nome]),
       menu_jogador(ID, Nome, Saldo)
    ;  writeln("ID não encontrado."), menu_principal
    ).

% --------- Criar conta ----------
criar_conta :-
    write("Digite seu nome: "), flush_output,
    read_line_to_string(user_input, Nome),
    estado_global:adicionar_jogador(Nome, ID),
    format("Sua conta foi criada! Seu ID é ~w~n", [ID]),
    menu_principal.

% --------- Menu do jogador ----------
menu_jogador(ID, Nome, Saldo) :-
    nl, format("=== Menu de ~w ===~n", [Nome]),
    writeln("1 - Blackjack"),
    writeln("2 - Baccarat (em breve)"),
    writeln("3 - Roleta (em breve)"),
    writeln("4 - Caça-níquel (em breve)"),
    writeln("5 - Caixa-surpresa (em breve)"),
    writeln("6 - Mostrar saldo"),
    writeln("7 - Mostrar ranking"),
    writeln("0 - Logout"),
    write("Escolha: "), flush_output,
    read_line_to_string(user_input, Input),
    handle_jogador_menu(Input, ID, Nome, Saldo).

% --------- Menu jogador handling ----------
handle_jogador_menu("1", ID, Nome, Saldo) :-
    jogar_blackjack_menu(ID, Nome, Saldo).

handle_jogador_menu("2", _, _, _) :- writeln("Baccarat ainda não implementado."), fail.
handle_jogador_menu("3", _, _, _) :- writeln("Roleta ainda não implementada."), fail.
handle_jogador_menu("4", _, _, _) :- writeln("Caça-níquel ainda não implementado."), fail.
handle_jogador_menu("5", _, _, _) :- writeln("Caixa-surpresa ainda não implementada."), fail.

handle_jogador_menu("6", ID, Nome, Saldo) :-
    estado_global:mostrar_saldo(ID),
    menu_jogador(ID, Nome, Saldo).

handle_jogador_menu("7", ID, Nome, Saldo) :-
    estado_global:mostrar_ranking,
    menu_jogador(ID, Nome, Saldo).

handle_jogador_menu("0", _, _, _) :-
    writeln("Logout realizado."), menu_principal.

handle_jogador_menu(_, ID, Nome, Saldo) :-
    writeln("Opção inválida."),
    menu_jogador(ID, Nome, Saldo).

% --------- Wrapper para Blackjack ----------
jogar_blackjack_menu(ID, Nome, Saldo) :-
    blackjack:blackjack_menu(ID, JogarNovamente),
    ( JogarNovamente == sim ->
        jogar_blackjack_menu(ID, Nome, Saldo)  % repete o menu se jogador quer jogar de novo
    ; writeln("Voltando ao menu do jogador..."),
      menu_jogador(ID, Nome, Saldo)
    ).

:- initialization(main).
