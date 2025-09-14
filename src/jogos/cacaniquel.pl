:- module(cacaniquel, [
    cacaniquel_menu/2
]).

:- use_module('../estado_global.pl').
:- use_module(library(random)).

% ===== Configurações =====
valor_aposta_cacaniquel(10.0).

% Símbolos possíveis
simbolo(tigre).
simbolo(cereja).
simbolo(ouro).
simbolo(limao).
simbolo(flor).
simbolo(estrela).

% Multiplicadores de prêmio
multiplicador(tigre, 250.0).
multiplicador(cereja, 100.0).
multiplicador(ouro, 10.0).
multiplicador(limao, 8.0).
multiplicador(flor, 5.0).
multiplicador(estrela, 3.0).

% ===== Geração dos rolos =====
gerar_rolo(S) :- 
    findall(Sim, simbolo(Sim), Lista),
    length(Lista, N),
    random(0, N, I),
    nth0(I, Lista, S).

girar_roleta([S1, S2, S3]) :-
    gerar_rolo(S1),
    gerar_rolo(S2),
    gerar_rolo(S3).

% ===== Verificação de vitória =====
calcular_ganho([S, S, S], Ganho) :-
    multiplicador(S, Mult),
    valor_aposta_cacaniquel(V),
    Ganho is V * Mult.
calcular_ganho(_, 0).

% ===== Interface do Jogo =====
exibir_multiplicadores :-
    writeln('------ Multiplicadores dos Símbolos ------'),
    forall(simbolo(S), (
        simbolo_emoji(S, Emoji),
        multiplicador(S, Mult),
        format('~w (~w): x~2f~n', [Emoji, S, Mult])
    )),
    writeln('------------------------------------------').

cacaniquel_menu(ID, JogarNovamente) :-
    estado_global:buscar_jogador_por_id(ID, jogador(ID, Nome, _, _, _, Saldo)),
    !,
    nl, writeln('=============================================='),
    writeln('🎰         ===== CAÇA-NÍQUEL =====         🎰'),
    writeln('=============================================='),
    format('Jogador: ~w~n', [Nome]),
    format('Saldo atual: R$ ~2f~n', [Saldo]),
    valor_aposta_cacaniquel(V),
    format('Valor da aposta: R$ ~2f~n', [V]),
    exibir_multiplicadores, % <-- Adiciona a tabela aqui
    writeln('----------------------------------------------'),
    (Saldo < V ->
        writeln('❌ Saldo insuficiente para jogar!'),
        writeln('=============================================='), nl,
        JogarNovamente = nao
    ;
        jogar_cacaniquel(ID, JogarNovamente)
    ).
cacaniquel_menu(_ID, nao) :-
    nl, writeln('=============================================='),
    writeln('❌ Erro: Jogador não encontrado.'),
    writeln('=============================================='), nl.

animacao_giro_final([S1, S2, S3]) :-
    Simbolos = ['🐯', '🍒', '🪙', '🍋', '🌸', '⭐'],
    % Rolo 1
    animacao_rolo_escrever(Simbolos, S1),
    nl,
    sleep(0.2),
    % Rolo 2
    animacao_rolo_escrever(Simbolos, S2),
    nl,
    sleep(0.2),
    % Rolo 3
    animacao_rolo_escrever(Simbolos, S3),
    nl,
    sleep(0.2).

animacao_rolo_escrever(Simbolos, SimboloFinal) :-
    % Mostra vários símbolos, apagando cada um, até o final
    forall(between(1, 10, I), (
        Indice is (I mod 6),
        nth0(Indice, Simbolos, Simbolo),
        write(Simbolo), flush_output,
        sleep(0.07),
        write('\b\b'), flush_output
    )),
    % Mostra o símbolo final
    simbolo_emoji(SimboloFinal, EmojiFinal),
    write(EmojiFinal), flush_output.


jogar_cacaniquel(ID, JogarNovamente) :-
    valor_aposta_cacaniquel(V),
    writeln('Girando os rolos...'),
    girar_roleta(Rolos),
    animacao_giro_final(Rolos),
    writeln('----------------------------------------------'),
    mostrar_rolos(Rolos),
    writeln('----------------------------------------------'),
    calcular_ganho(Rolos, Ganho),
    estado_global:registrar_jogada(ID, 'Caca-niquel', V, Ganho),
    mostrar_resultado_cacaniquel(Rolos, Ganho),
    writeln('----------------------------------------------'),
    estado_global:mostrar_saldo(ID),
    writeln('=============================================='),
    perguntar_jogar_novamente(JogarNovamente).


mostrar_rolos([S1, S2, S3]) :-
    simbolo_emoji(S1, E1), 
    simbolo_emoji(S2, E2), 
    simbolo_emoji(S3, E3),
    format('Resultado: ~w | ~w | ~w~n', [E1, E2, E3]).

% Emojis para os símbolos
simbolo_emoji(tigre, '🐯').
simbolo_emoji(cereja, '🍒').
simbolo_emoji(ouro, '🪙').
simbolo_emoji(limao, '🍋').
simbolo_emoji(flor, '🌸').
simbolo_emoji(estrela, '⭐').

mostrar_resultado_cacaniquel([S, S, S], Ganho) :-
    !,
    multiplicador(S, Mult),
    format('Parabéns! Trinca de ~w! Multiplicador: ~2f~n', [S, Mult]),
    format('Você ganhou: ~2f~n', [Ganho]).
mostrar_resultado_cacaniquel(_, 0) :-
    valor_aposta_cacaniquel(V),
    writeln('💔 Não foi dessa vez. Tente novamente!'),
    format('Você perdeu: ~2f~n', [V]).

animacao_giro :-
    Simbolos = ['🐯', '🍒', '🪙', '🍋', '🌸', '⭐'],
    forall(between(1, 12, I), (
        Indice is (I mod 6),
        nth0(Indice, Simbolos, Simbolo),
        write(Simbolo), write(' '),
        flush_output,
        sleep(0.12)
    )),
    nl.

perguntar_jogar_novamente(JogarNovamente) :-
    nl,
    write("Quer jogar caça-níquel novamente? (s/n): "), flush_output,
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