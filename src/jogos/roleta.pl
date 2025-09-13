:- module(roleta, [
    roleta_menu/2
]).

:- use_module('../estado_global.pl').
:- use_module(library(readutil)).
:- use_module(library(random)).

% ===== Definições da Roleta =====

% Cores disponíveis
cor_roleta(vermelho).
cor_roleta(preto).
cor_roleta(verde).

% Multiplicadores para cada cor
multiplicador_roleta(vermelho, 2.0).
multiplicador_roleta(preto, 2.0).
multiplicador_roleta(verde, 15.0).

valor_aposta_roleta(10.0).

% Distribuição das cores com peso (47 vermelho, 47 preto, 6 verde)
cores_com_peso(Cores) :-
    findall(vermelho, between(1, 47, _), Vermelhos),
    findall(preto, between(1, 47, _), Pretos),
    findall(verde, between(1, 6, _), Verdes),
    append(Vermelhos, Pretos, Temp),
    append(Temp, Verdes, Cores).

% ===== Lógica do Jogo =====
girar_roleta(CorSorteada) :-
    cores_com_peso(Cores),
    length(Cores, Tamanho),
    Max is Tamanho - 1,
    random(0, Max, Indice),
    nth0(Indice, Cores, CorSorteada).

verificar_vitoria(CorEscolhida, CorSorteada) :-
    CorEscolhida = CorSorteada.

calcular_ganho(CorEscolhida, CorSorteada, Ganho) :-
    valor_aposta_roleta(ValorAposta),
    (   verificar_vitoria(CorEscolhida, CorSorteada) ->
        multiplicador_roleta(CorEscolhida, Mult),
        Ganho is ValorAposta * Mult
    ;   Ganho = 0  % Jogador perde a aposta, ganho = 0
    ).

% ===== Interface do Jogo =====
roleta_menu(ID, JogarNovamente) :-
    estado_global:buscar_jogador_por_id(ID, Jogador),
    ( Jogador = jogador(ID, Nome, _, _, _, Saldo) ->
        mostrar_cabecalho_roleta(Nome, Saldo),
        valor_aposta_roleta(ValorAposta),
        ( Saldo >= ValorAposta ->
            escolher_cor(CorEscolhida),
            ( CorEscolhida \= sair ->
                jogar_rodada(ID, CorEscolhida),
                perguntar_jogar_novamente(JogarNovamente)
            ; JogarNovamente = nao
            )
        ; writeln("❌ Saldo insuficiente para apostar!"),
          format("Você precisa de pelo menos ~2f para jogar.~n", [ValorAposta]),
          JogarNovamente = nao
        )
    ; writeln("❌ Erro: Jogador não encontrado."),
      JogarNovamente = nao
    ).

mostrar_cabecalho_roleta(Nome, Saldo) :-
    nl,
    writeln("🎰 ===== ROLETA DA SORTE ===== 🎰"),
    format("Jogador: ~w | Saldo: ~2f~n", [Nome, Saldo]),
    valor_aposta_roleta(ValorAposta),
    format("Valor da aposta: ~2f~n", [ValorAposta]),
    writeln(""),
    writeln("💰 MULTIPLICADORES:"),
    writeln("🔴 Vermelho: 2.0x (47% de chance)"),
    writeln("⚫ Preto: 2.0x (47% de chance)"),
    writeln("🟢 Verde: 15.0x (6% de chance)"),
    writeln(""),
    writeln("Escolha sua cor da sorte!"),
    nl.

escolher_cor(CorEscolhida) :-
    writeln("1 - 🔴 Vermelho"),
    writeln("2 - ⚫ Preto"),
    writeln("3 - 🟢 Verde"),
    writeln("0 - ⬅️  Voltar ao menu"),
    write("Sua escolha: "), flush_output,
    read_line_to_string(user_input, Input),
    processar_escolha_cor(Input, CorEscolhida).

processar_escolha_cor("1", vermelho).
processar_escolha_cor("2", preto).
processar_escolha_cor("3", verde).
processar_escolha_cor("0", sair).
processar_escolha_cor(_, CorEscolhida) :-
    writeln("❌ Opção inválida! Tente novamente."),
    nl,
    escolher_cor(CorEscolhida).

jogar_rodada(ID, CorEscolhida) :-
    valor_aposta_roleta(ValorAposta),
    
    writeln("🎲 Girando a roleta..."),
    animacao_giro,
    
    girar_roleta(CorSorteada),
    
    mostrar_resultado_roleta(CorEscolhida, CorSorteada),
    
    calcular_ganho(CorEscolhida, CorSorteada, Ganho),
    
    estado_global:registrar_jogada(ID, "Roleta", ValorAposta, Ganho),
    
    mostrar_resultado_final(CorEscolhida, CorSorteada, ValorAposta, Ganho),
    
    estado_global:mostrar_saldo(ID).

animacao_giro :-
    writeln("*** Girando a roleta..."),
    Simbolos = ['*', 'o', '+', '#'],
    forall(between(1, 12, I), (
        Indice is (I mod 4),
        nth0(Indice, Simbolos, Simbolo),
        write(Simbolo), write(' '),
        flush_output,
        sleep(0.15)
    )),
    nl.

mostrar_resultado_roleta(CorEscolhida, CorSorteada) :-
    nl,
    writeln("🎯 ===== RESULTADO ====="),
    format("Você apostou em: ~w~n", [CorEscolhida]),
    emoji_cor(CorSorteada, Emoji),
    format("A roleta parou em: ~w ~w~n", [Emoji, CorSorteada]),
    nl.

mostrar_resultado_final(CorEscolhida, CorSorteada, ValorAposta, Ganho) :-
    ( verificar_vitoria(CorEscolhida, CorSorteada) ->
        multiplicador_roleta(CorEscolhida, Mult),
        writeln("🎉 ===== PARABÉNS! VOCÊ GANHOU! ===== 🎉"),
        format("*** Multiplicador: ~1fx~n", [Mult]),
        format("*** Ganho líquido: ~2f~n", [Ganho - ValorAposta])
    ; writeln("💔 ===== QUE PENA! VOCÊ PERDEU! ===== 💔"),
      format("*** Perda: ~2f~n", [ValorAposta])
    ),
    nl.

emoji_cor(vermelho, "🔴").
emoji_cor(preto, "⚫").
emoji_cor(verde, "🟢").

perguntar_jogar_novamente(JogarNovamente) :-
    nl,
    write("Quer jogar roleta novamente? (s/n): "), flush_output,
    read_line_to_string(user_input, Input),
    processar_resposta_jogar_novamente(Input, JogarNovamente).

processar_resposta_jogar_novamente("s", sim).
processar_resposta_jogar_novamente("S", sim).
processar_resposta_jogar_novamente("sim", sim).
processar_resposta_jogar_novamente("SIM", sim).
processar_resposta_jogar_novamente("n", nao).
processar_resposta_jogar_novamente("N", nao).
processar_resposta_jogar_novamente("nao", nao).
processar_resposta_jogar_novamente("não", nao).
processar_resposta_jogar_novamente("NAO", nao).
processar_resposta_jogar_novamente("NÃO", nao).
processar_resposta_jogar_novamente(_, JogarNovamente) :-
    writeln("❌ Resposta inválida! Digite 's' para sim ou 'n' para não."),
    perguntar_jogar_novamente(JogarNovamente).