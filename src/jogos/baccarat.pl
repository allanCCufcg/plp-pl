:- module(baccarat, [
    baccarat_menu/2
]).

:- use_module('../estado_global.pl').
:- use_module(library(random)).
:- use_module(library(readutil)).

% ===== Definições do Baccarat =====
valor_aposta_baccarat(10.0).

% Multiplicadores para cada tipo de aposta
multiplicador_baccarat(jogador, 2.0).
multiplicador_baccarat(banco, 2.0).
multiplicador_baccarat(empate, 8.0).

% ===== Lógica do Baccarat =====
% Valores das cartas no Baccarat (0-9)
valor_baccarat(as, 1).
valor_baccarat(2, 2).
valor_baccarat(3, 3).
valor_baccarat(4, 4).
valor_baccarat(5, 5).
valor_baccarat(6, 6).
valor_baccarat(7, 7).
valor_baccarat(8, 8).
valor_baccarat(9, 9).
valor_baccarat(10, 0).
valor_baccarat(valete, 0).
valor_baccarat(dama, 0).
valor_baccarat(rei, 0).

% Lista de todos os valores possíveis
valores_baccarat([as, 2, 3, 4, 5, 6, 7, 8, 9, 10, valete, dama, rei]).

% Calcular pontuação (só o dígito da unidade)
calcular_pontuacao(Mao, Pontuacao) :-
    maplist(valor_baccarat, Mao, Valores),
    sum_list(Valores, Soma),
    Pontuacao is Soma mod 10.

% Gerar carta aleatória
carta_aleatoria(Carta) :-
    valores_baccarat(Valores),
    length(Valores, Tamanho),
    Max is Tamanho - 1,
    random(0, Max, Indice),
    nth0(Indice, Valores, Carta).

% Distribuir cartas
distribuir_cartas([C1, C2]) :-
    carta_aleatoria(C1),
    carta_aleatoria(C2).

% Determinar vencedor
determinar_vencedor(PontosJogador, PontosBanco, Vencedor) :-
    (PontosJogador > PontosBanco -> Vencedor = jogador;
     PontosBanco > PontosJogador -> Vencedor = banco;
     Vencedor = empate).

% Calcular ganho
calcular_ganho_baccarat(Aposta, Resultado, Ganho) :-
    valor_aposta_baccarat(ValorAposta),
    (Aposta = Resultado ->
        multiplicador_baccarat(Aposta, Mult),
        Ganho is ValorAposta * Mult
    ; Ganho = 0).

% ===== Interface do Baccarat =====
baccarat_menu(ID, JogarNovamente) :-
    estado_global:buscar_jogador_por_id(ID, Jogador),
    (Jogador = jogador(ID, Nome, _, _, _, Saldo) ->
        mostrar_cabecalho_baccarat(Nome, Saldo),
        valor_aposta_baccarat(ValorAposta),
        (Saldo >= ValorAposta ->
            escolher_aposta_baccarat(Aposta),
            (Aposta \= sair ->
                jogar_rodada_baccarat(ID, Aposta),
                perguntar_jogar_novamente_baccarat(JogarNovamente)
            ; JogarNovamente = nao
            )
        ; writeln("❌ Saldo insuficiente para apostar!"),
          format("Você precisa de pelo menos ~2f para jogar.~n", [ValorAposta]),
          JogarNovamente = nao
        )
    ; writeln("❌ Erro: Jogador não encontrado."),
      JogarNovamente = nao
    ).

mostrar_cabecalho_baccarat(Nome, Saldo) :-
    nl,
    writeln("🎴 ===== BACCARAT ===== 🎴"),
    format("Jogador: ~w | Saldo: ~2f~n", [Nome, Saldo]),
    valor_aposta_baccarat(ValorAposta),
    format("Valor da aposta: ~2f~n", [ValorAposta]),
    writeln(""),
    writeln("💰 MULTIPLICADORES:"),
    writeln("👤 Jogador: 2.0x"),
    writeln("🏦 Banco: 2.0x"),
    writeln("🤝 Empate: 8.0x"),
    writeln(""),
    writeln("Faça sua aposta!"),
    nl.

escolher_aposta_baccarat(Aposta) :-
    writeln("1 - 👤 Jogador (2.0x)"),
    writeln("2 - 🏦 Banco (2.0x)"),
    writeln("3 - 🤝 Empate (8.0x)"),
    writeln("0 - ⬅️  Voltar ao menu"),
    write("Sua escolha: "), flush_output,
    read_line_to_string(user_input, Input),
    processar_escolha_aposta(Input, Aposta).

processar_escolha_aposta("1", jogador).
processar_escolha_aposta("2", banco).
processar_escolha_aposta("3", empate).
processar_escolha_aposta("0", sair).
processar_escolha_aposta(_, Aposta) :-
    writeln("❌ Opção inválida! Tente novamente."),
    nl,
    escolher_aposta_baccarat(Aposta).

jogar_rodada_baccarat(ID, Aposta) :-
    valor_aposta_baccarat(ValorAposta),
    
    writeln("🎴 Distribuindo cartas..."),
    sleep(1),
    
    % Distribuir cartas para jogador e banco
    distribuir_cartas(MaoJogador),
    distribuir_cartas(MaoBanco),
    
    % Calcular pontuações
    calcular_pontuacao(MaoJogador, PontosJogador),
    calcular_pontuacao(MaoBanco, PontosBanco),
    
    % Mostrar resultados
    mostrar_resultado_baccarat(MaoJogador, MaoBanco, PontosJogador, PontosBanco),
    
    % Determinar vencedor
    determinar_vencedor(PontosJogador, PontosBanco, Resultado),
    
    % Calcular ganho
    calcular_ganho_baccarat(Aposta, Resultado, Ganho),
    
    % Registrar jogada
    estado_global:registrar_jogada(ID, "Baccarat", ValorAposta, Ganho),
    
    % Mostrar resultado final
    mostrar_resultado_final_baccarat(Aposta, Resultado, ValorAposta, Ganho),
    
    estado_global:mostrar_saldo(ID).

mostrar_resultado_baccarat(MaoJogador, MaoBanco, PontosJogador, PontosBanco) :-
    nl,
    writeln("🎯 ===== RESULTADO ====="),
    mostrar_mao_baccarat(MaoJogador, "Jogador", PontosJogador),
    mostrar_mao_baccarat(MaoBanco, "Banco", PontosBanco),
    nl.

mostrar_mao_baccarat(Mao, Nome, Pontos) :-
    maplist(formatar_carta, Mao, CartasStr),
    atomic_list_concat(CartasStr, " ", MaoStr),
    format("~w: ~s (Pontos: ~d)~n", [Nome, MaoStr, Pontos]).

formatar_carta(as, "A").
formatar_carta(10, "10").
formatar_carta(valete, "J").
formatar_carta(dama, "Q").
formatar_carta(rei, "K").
formatar_carta(Carta, Str) :- number(Carta), number_string(Carta, Str).

mostrar_resultado_final_baccarat(Aposta, Resultado, ValorAposta, Ganho) :-
    (Aposta = Resultado ->
        format("🎉 PARABÉNS! Você acertou a aposta em ~w!~n", [Resultado]),
        format("💰 Ganho líquido: ~2f~n", [Ganho - ValorAposta])
    ; format("💔 Que pena! A aposta vencedora era ~w.~n", [Resultado]),
      format("💸 Perda: ~2f~n", [ValorAposta])
    ),
    nl.

perguntar_jogar_novamente_baccarat(JogarNovamente) :-
    nl,
    write("Quer jogar Baccarat novamente? (s/n): "), flush_output,
    read_line_to_string(user_input, Input),
    processar_resposta_jogar_novamente_baccarat(Input, JogarNovamente).

processar_resposta_jogar_novamente_baccarat("s", sim).
processar_resposta_jogar_novamente_baccarat("S", sim).
processar_resposta_jogar_novamente_baccarat("sim", sim).
processar_resposta_jogar_novamente_baccarat("SIM", sim).
processar_resposta_jogar_novamente_baccarat("n", nao).
processar_resposta_jogar_novamente_baccarat("N", nao).
processar_resposta_jogar_novamente_baccarat("nao", nao).
processar_resposta_jogar_novamente_baccarat("não", nao).
processar_resposta_jogar_novamente_baccarat("NAO", nao).
processar_resposta_jogar_novamente_baccarat("NÃO", nao).
processar_resposta_jogar_novamente_baccarat(_, JogarNovamente) :-
    writeln("❌ Resposta inválida! Digite 's' para sim ou 'n' para não."),
    perguntar_jogar_novamente_baccarat(JogarNovamente).
