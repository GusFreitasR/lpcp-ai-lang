# lpcp-ai-lang
Este repositório contém a especificação formal e o protótipo do interpretador (desenvolvido em **Haskell**) de uma linguagem de programação imperativa voltada para o domínio de **Inteligência Artificial e Aprendizado de Máquina**.

## Componentes principais

- **`corbieProgrammingLanguage/`**: protótipo de uma linguagem simples.
  - `scanner.x`: especificação do analisador léxico. O Alex transforma esse
    arquivo em código Haskell e gera tokens para palavras-chave,
    identificadores, inteiros e operadores.
  - `parserV0.hs`: analisador sintático construído com Parsec. Atualmente
    reconhece programas com `program`, `begin`, `end` e atribuições inteiras
    como `x := 1`.
  - `programaV0.pe`: programa de exemplo usado como entrada.
- **`CSV/`**: duas implementações didáticas de um parser para arquivos CSV,
  usando combinadores do Parsec.
- **`userState/`**: exemplos de estado interno em parsers. Eles calculam uma
  expressão de soma e contam quantos números foram reconhecidos.

## Requisitos

- GHC/GHCi (compilador e interpretador Haskell);
- Cabal (gerenciador de pacotes);
- Alex (gerador do analisador léxico);
- pacote Haskell `parsec`.

### Instalação no Windows

Uma forma simples de instalar o ambiente Haskell é usar o
[GHCup](https://www.haskell.org/ghcup/). Durante a instalação, selecione GHC,
Cabal e, se disponível, Alex.

Verifique as ferramentas no PowerShell:

```powershell
ghc --version
ghci --version
cabal --version
alex --version
```

Caso o Alex não esteja instalado:

```powershell
cabal update
cabal install alex
```

O Parsec pode ser instalado, se necessário, com:

```powershell
cabal install parsec
```

## Executando a linguagem de exemplo

No PowerShell, entre na pasta da linguagem:

```powershell
cd "C:\caminho\para\parserV0\toyProgrammingLanguage"
```

Gere o scanner:

```powershell
alex scanner.x
```

Compile e execute o parser:

```powershell
ghc parserV0.hs scanner.hs -o parserV0.exe
.\parserV0.exe
```

O programa lê `programaV0.pe`, tokeniza seu conteúdo e imprime o resultado do
parsing. Um exemplo válido é:

```text
program main
begin
  x := 1;
  y := 2
end
```

Os arquivos `scanner.hs`, `.o`, `.hi` e `.exe` são gerados durante o processo
e não precisam ser versionados.

## Executando os exemplos de CSV

Os arquivos CSV não possuem uma função `main`; use o GHCi:

```powershell
cd "C:\caminho\para\parserV0"
ghci CSV\csv2.hs
```

No GHCi:

```haskell
parseCSV "a,b\nc,d\n"
```

Resultado esperado:

```haskell
Right [["a","b"],["c","d"]]
```

`CSV\csv1.hs` pode ser testado da mesma forma.

## Executando os exemplos de estado

```powershell
ghci userState\user_state.hs
```

No GHCi:

```haskell
play "12+3+4"
```

Resultado esperado:

```haskell
Right (19,3)
```

O segundo exemplo pode ser executado com:

```powershell
ghci userState\user_state_infix.hs
```

Para sair do GHCi, use `:q`.
