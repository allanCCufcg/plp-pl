:- module(estado_global, [
    inicializar_sistema/0,
    carregar_dados/0,
    salvar_dados/0,
    salvar_dados_seguro/0,
    adicionar_jogador/2,
    listar_jogadores/0,
    mostrar_ranking/0,
    registrar_jogada/4,
    mostrar_saldo/1,
    buscar_jogador_por_id/2,
    adicionar_saldo/2,
    remover_saldo/2
]).

:- use_module(library(http/json)).
:- use_module(library(filesex)).   % make_directory_path/1
:- use_module(library(random)).
:- dynamic jogador/6.              % jogador(ID, Nome, TotalApostas, TotalGanho, Historico, Saldo)
:- dynamic configuracao/2.         % configuracao(saldo_inicial, aposta_minima)

% ===== Caminho do arquivo =====
arquivo_dados("data/dados_jogo.json").

% ===== Inicialização =====
inicializar_sistema :-
    make_directory_path("data"),
    ( exists_file("data/dados_jogo.json")
    -> carregar_dados
    ;  assertz(configuracao(saldo_inicial, 100)),
       assertz(configuracao(aposta_minima, 5)),
       salvar_dados
    ),
    findall(ID, jogador(ID,_,_,_,_,_), Jogadores),
    length(Jogadores, N),
    format("[INIT] Sistema carregado com ~w jogadores.~n", [N]).

% ===== Carregar / Salvar =====
carregar_dados :-
    arquivo_dados(Arq),
    catch(open(Arq, read, Stream), _, fail),
    json_read_dict(Stream, Dict),
    close(Stream),
    retractall(jogador(_,_,_,_,_,_)),
    retractall(configuracao(_,_)),
    assertz(configuracao(saldo_inicial, Dict.configs.saldoInicial)),
    assertz(configuracao(aposta_minima, Dict.configs.apostaMinima)),
    forall(member(J, Dict.jogadores),
           assertz(jogador(J.playerID, J.nome, J.totalApostas, J.totalGanho, J.historico, J.saldo))),
    writeln("[LOAD] Dados carregados do JSON com sucesso.").

salvar_dados :-
    arquivo_dados(Arq),
    make_directory_path("data"),
    findall(_{ playerID:ID, nome:Nome, totalApostas:TA, totalGanho:TG, historico:Hist, saldo:Saldo },
            jogador(ID,Nome,TA,TG,Hist,Saldo),
            Jogadores),
    findall(_{ saldoInicial:V } , configuracao(saldo_inicial,V), [SaldoInicial]),
    findall(_{ apostaMinima:V } , configuracao(aposta_minima,V), [ApostaMinima]),
    Configs = _{ saldoInicial:SaldoInicial.saldoInicial, apostaMinima:ApostaMinima.apostaMinima },
    Dados = _{ jogadores:Jogadores, configs:Configs },
    open(Arq, write, Stream),
    json_write_dict(Stream, Dados, [width(128)]),
    close(Stream),
    writeln("[SAVE] Dados persistidos em JSON.").

salvar_dados_seguro :-
    catch(salvar_dados, Err,
          ( writeln("[ERROR] Falha ao salvar dados."), print_message(error, Err) )).

% ===== Jogadores =====
gerar_id_unico(ID) :-
    random_between(1000, 9999, ID),
    \+ jogador(ID,_,_,_,_,_).

adicionar_jogador(Nome, ID) :-
    gerar_id_unico(ID),
    configuracao(saldo_inicial, SaldoInicial),
    assertz(jogador(ID, Nome, 0, 0.0, [], SaldoInicial)),
    salvar_dados_seguro,
    format("[NEW PLAYER] Jogador ~w criado com ID ~w e saldo inicial ~w.~n", [Nome, ID, SaldoInicial]).

listar_jogadores :-
    writeln("[PLAYERS] === Jogadores cadastrados ==="),
    forall(jogador(ID, Nome, _, _, _, Saldo),
           format("ID ~w: ~w (Saldo: ~2f)~n", [ID, Nome, Saldo])).

buscar_jogador_por_id(ID, Jogador) :-
    ( jogador(ID, Nome, TA, TG, Hist, Saldo)
    -> Jogador = jogador(ID, Nome, TA, TG, Hist, Saldo)
    ;  Jogador = none
    ).

mostrar_saldo(ID) :-
    ( jogador(ID, Nome, _, _, _, Saldo)
    -> format("[BALANCE] Saldo de ~w: ~2f~n", [Nome, Saldo])
    ;  writeln("[ERROR] Jogador não encontrado.")
    ).

mostrar_ranking :-
    findall(jogador(ID, Nome, _, _, _, Saldo),
            jogador(ID, Nome, _, _, _, Saldo),
            Jogadores),
    % Ordena pelo saldo decrescente
    sort(6, @>=, Jogadores, Ranking),
    writeln("[RANKING] === TOP Jogadores ==="),
    mostrar_ranking_lista(Ranking, 1).

mostrar_ranking_lista([], _).
mostrar_ranking_lista([jogador(_, Nome, _, _, _, Saldo)|Rest], Pos) :-
    format("~wº ~w - Saldo atual: ~2f~n", [Pos, Nome, Saldo]),
    Next is Pos + 1,
    mostrar_ranking_lista(Rest, Next).

% ===== Atualização de stats =====
registrar_jogada(ID, Jogo, Valor, Ganho) :-
    ( retract(jogador(ID, Nome, TA, TG, Hist, Saldo)) ->
        NovoTA is TA + 1,
        NovoTG is TG + Ganho,
        NovoHist = [_{jogo: Jogo, aposta: Valor}|Hist],
        NovoSaldo is max(0, Saldo - Valor + Ganho),
        assertz(jogador(ID, Nome, NovoTA, NovoTG, NovoHist, NovoSaldo)),
        salvar_dados_seguro,
        format("[GAME] Jogador ~w jogou ~w (aposta: ~w, ganho: ~w).~n", [Nome, Jogo, Valor, Ganho])
    ; writeln("[ERROR] Jogador não encontrado.")
    ).

adicionar_saldo(ID, Valor) :-
    ( retract(jogador(ID, Nome, TA, TG, Hist, Saldo)) ->
        NovoSaldo is Saldo + Valor,
        assertz(jogador(ID, Nome, TA, TG, Hist, NovoSaldo)),
        salvar_dados_seguro,
        format("[MONEY] ~w recebeu ~2f. Novo saldo: ~2f~n", [Nome, Valor, NovoSaldo])
    ; writeln("[ERROR] Jogador não encontrado.")
    ).

remover_saldo(ID, Valor) :-
    ( retract(jogador(ID, Nome, TA, TG, Hist, Saldo)) ->
        NovoSaldo is max(0, Saldo - Valor),
        assertz(jogador(ID, Nome, TA, TG, Hist, NovoSaldo)),
        salvar_dados_seguro,
        format("[MONEY] ~w perdeu ~2f. Novo saldo: ~2f~n", [Nome, Valor, NovoSaldo])
    ; writeln("[ERROR] Jogador não encontrado.")
    ).
