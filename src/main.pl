:- set_prolog_flag(encoding, utf8).
:- use_module('estado_global.pl').
:- use_module('jogos/blackjack.pl').
:- use_module('jogos/roleta.pl').
:- use_module('jogos/cacaniquel.pl').
:- use_module('jogos/caixasurpresa.pl').
:- use_module(library(readutil)).

% --------- Menu principal ----------
main :-
    estado_global:inicializar_sistema,
    menu_principal.

menu_principal :-
    nl, writeln("==================================="),
    writeln("   Bem-vindo ao Cassino Prolog"),
    writeln("==================================="),
    writeln("1 - Login"),
    writeln("2 - Criar conta"),
    writeln("3 - Listar jogadores"),
    writeln("4 - Mostrar ranking"),
    writeln("0 - Sair"),
    write("Escolha: "), flush_output,
    read_line_to_string(user_input, Input),
    handle_main_menu(Input).

handle_main_menu("1") :- login_robusto.
handle_main_menu("2") :- criar_conta.
handle_main_menu("3") :- estado_global:listar_jogadores, menu_principal.
handle_main_menu("4") :- estado_global:mostrar_ranking, menu_principal.
handle_main_menu("0") :- writeln("Saindo...").
handle_main_menu(_) :- writeln("Opcao invalida."), menu_principal.

% --------- Login CORRIGIDO ----------
login_robusto :-
    write("Digite seu ID: "), flush_output,
    read_line_to_string(user_input, Input),
    processar_login(Input).

processar_login(Input) :-
    % Limpar string de espaços
    string_trim(Input, InputLimpo),
    
    % Verificar se não está vazio
    ( InputLimpo = "" ->
        writeln("ID nao pode estar vazio!"),
        menu_principal
    ;
        % Tentar converter para número
        catch(
            (number_string(ID, InputLimpo), fazer_login(ID)),
            Error,
            tratar_erro_login(InputLimpo, Error)
        )
    ).

fazer_login(ID) :-
    estado_global:buscar_jogador_por_id(ID, Jogador),
    ( Jogador \= none ->
        Jogador = jogador(ID, Nome, _, _, _, Saldo),
        nl,
        writeln("===== LOGIN BEM-SUCEDIDO! ====="),
        format("Jogador: ~w~n", [Nome]),
        format("ID: ~w~n", [ID]),
        format("Saldo atual: ~2f~n", [Saldo]),
        nl,
        menu_jogador(ID, Nome, Saldo)
    ;
        format("Jogador com ID ~w nao foi encontrado.~n", [ID]),
        writeln(""),
        writeln("Jogadores disponiveis:"),
        estado_global:listar_jogadores,
        nl,
        menu_principal
    ).

tratar_erro_login(Input, _Error) :-
    format("'~w' nao e um numero valido!~n", [Input]),
    writeln("O ID deve conter apenas numeros (exemplo: 1234)"),
    nl,
    menu_principal.

% --------- Função auxiliar para trim ----------
string_trim(String, Trimmed) :-
    % Remove espaços do início e fim
    atom_string(Atom, String),
    atom_codes(Atom, Codes),
    trim_codes(Codes, TrimmedCodes),
    atom_codes(TrimmedAtom, TrimmedCodes),
    atom_string(TrimmedAtom, Trimmed).

trim_codes(Codes, Trimmed) :-
    trim_left(Codes, LeftTrimmed),
    reverse(LeftTrimmed, Reversed),
    trim_left(Reversed, RightTrimmed),
    reverse(RightTrimmed, Trimmed).

trim_left([32|Rest], Trimmed) :- !, trim_left(Rest, Trimmed).
trim_left([9|Rest], Trimmed) :- !, trim_left(Rest, Trimmed).   % Tab
trim_left([10|Rest], Trimmed) :- !, trim_left(Rest, Trimmed).  % Newline
trim_left([13|Rest], Trimmed) :- !, trim_left(Rest, Trimmed).  % Carriage return
trim_left(Codes, Codes).

% --------- Criar conta melhorada ----------
criar_conta :-
    write("Digite seu nome: "), flush_output,
    read_line_to_string(user_input, Nome),
    string_trim(Nome, NomeLimpo),
    ( NomeLimpo = "" ->
        writeln("Nome nao pode estar vazio!"),
        menu_principal
    ;
        estado_global:adicionar_jogador(NomeLimpo, ID),
        nl,
        writeln("===== CONTA CRIADA COM SUCESSO! ====="),
        format("Nome: ~w~n", [NomeLimpo]),
        format("Seu ID e: ~w~n", [ID]),
        writeln("Saldo inicial: 100.00"),
        writeln(""),
        writeln("Anote seu ID para fazer login!"),
        nl,
        menu_principal
    ).

% --------- Menu do jogador ----------
menu_jogador(ID, Nome, _SaldoAntigo) :-
    estado_global:buscar_jogador_por_id(ID, Jogador),
    ( Jogador = jogador(ID, Nome, _, _, _, Saldo) ->
        nl, format("=== Menu de ~w ===~n", [Nome]),
        format("Saldo atual: ~2f~n", [Saldo]),
        writeln(""),
        writeln("JOGOS DISPONIVEIS:"),
        writeln("1 - Blackjack"),
        writeln("2 - Baccarat (em breve)"),
        writeln("3 - Roleta"),
        writeln("4 - Caca-niquel"), 
        writeln("5 - Caixa-surpresa"),
        writeln(""),
        writeln("OUTRAS OPCOES:"),
        writeln("6 - Mostrar saldo detalhado"),
        writeln("7 - Mostrar ranking"),
        writeln("0 - Logout"),
        write("Escolha: "), flush_output,
        read_line_to_string(user_input, Input),
        handle_jogador_menu(Input, ID, Nome, Saldo)
    ;
        writeln("Erro: Jogador nao encontrado."),
        menu_principal
    ).

% --------- Menu jogador handling ----------
handle_jogador_menu("1", ID, Nome, Saldo) :-
    jogar_blackjack_menu(ID, Nome, Saldo).

handle_jogador_menu("2", ID, Nome, Saldo) :- 
    writeln("Baccarat ainda nao implementado."),
    writeln("Em breve disponivel!"),
    menu_jogador(ID, Nome, Saldo).

handle_jogador_menu("3", ID, Nome, Saldo) :-
    jogar_roleta_menu(ID, Nome, Saldo).

handle_jogador_menu("4", ID, Nome, Saldo) :- 
    jogar_cacaniquel_menu(ID, Nome, Saldo).

handle_jogador_menu("5", ID, Nome, Saldo) :-
    jogar_caixa_surpresa_menu(ID, Nome, Saldo).

handle_jogador_menu("6", ID, Nome, Saldo) :-
    nl,
    writeln("===== DETALHES DA CONTA ====="),
    estado_global:mostrar_saldo(ID),
    ( estado_global:buscar_jogador_por_id(ID, jogador(ID, Nome, TA, TG, Hist, _)) ->
        format("Total de jogadas: ~w~n", [TA]),
        format("Total ganho/perdido: ~2f~n", [TG]),
        length(Hist, NumJogadas),
        format("Jogos no historico: ~w~n", [NumJogadas])
    ; true
    ),
    writeln("==============================="),
    menu_jogador(ID, Nome, Saldo).

handle_jogador_menu("7", ID, Nome, Saldo) :-
    estado_global:mostrar_ranking,
    menu_jogador(ID, Nome, Saldo).

handle_jogador_menu("0", _, _, _) :-
    nl,
    writeln("Logout realizado com sucesso!"),
    writeln("Volte sempre ao Cassino Prolog!"),
    menu_principal.

handle_jogador_menu(_, ID, Nome, Saldo) :-
    writeln("Opcao invalida."),
    menu_jogador(ID, Nome, Saldo).

% --------- Wrapper para Blackjack ----------
jogar_blackjack_menu(ID, Nome, Saldo) :-
    blackjack:blackjack_menu(ID, JogarNovamente),
    ( JogarNovamente == sim ->
        jogar_blackjack_menu(ID, Nome, Saldo)
    ; writeln("Voltando ao menu do jogador..."),
      menu_jogador(ID, Nome, Saldo)
    ).

% --------- Wrapper para Roleta ----------
jogar_roleta_menu(ID, Nome, Saldo) :-
    roleta:roleta_menu(ID, JogarNovamente),
    ( JogarNovamente == sim ->
        jogar_roleta_menu(ID, Nome, Saldo)
    ; writeln("Voltando ao menu do jogador..."),
      menu_jogador(ID, Nome, Saldo)
    ).

% --------- Wrapper para Caça-níquel ----------
jogar_cacaniquel_menu(ID, Nome, Saldo) :-
    cacaniquel:cacaniquel_menu(ID, JogarNovamente),
    ( JogarNovamente == sim ->
        jogar_cacaniquel_menu(ID, Nome, Saldo)
    ; menu_jogador(ID, Nome, Saldo)
    ).

% --------- Wrapper para Caixa Surpresa ----------
jogar_caixa_surpresa_menu(ID, Nome, Saldo) :-
    caixasurpresa:caixa_surpresa_menu(ID, JogarNovamente),
    ( JogarNovamente == sim ->
        jogar_caixa_surpresa_menu(ID, Nome, Saldo)
    ; writeln("Voltando ao menu do jogador..."),
      menu_jogador(ID, Nome, Saldo)
    ).

% --------- Inicialização ----------
:- initialization(main).
