# Simulador de Sistema Solar

Um simulador de sistema solar com interface gráfica em Python que permite comparar implementações de física em diferentes paradigmas de programação: orientação a objetos (Python) e programação funcional (Haskell).

## 📋 Funcionalidades

- Interface gráfica interativa desenvolvida com Pygame
- Dois backends de simulação física:
  - **Python**: Implementação orientada a objetos
  - **Haskell**: Implementação funcional
- Comparação de performance entre paradigmas
- Visualização em tempo real da dinâmica orbital

## 🛠️ Pré-requisitos

### Para o Backend Python
- Python 3.8 ou superior
- pip (gerenciador de pacotes Python)

### Para o Backend Haskell
- GHC (Glasgow Haskell Compiler) 8.10 ou superior
- Cabal ou Stack (gerenciadores de build Haskell)

## 📦 Instalação

### 1. Clone o repositório ou baixe o projeto
```bash
git clone <url-do-repositorio>
cd simulador-sistema-solar
```

### 2. Configure o ambiente Python
```bash
# Crie um ambiente virtual (recomendado)
python -m venv venv

# Ative o ambiente virtual
# No Linux/Mac:
source venv/bin/activate
# No Windows:
venv\Scripts\activate

# Instale as dependências
pip install -r requirements.txt
```

### 3. Compile o backend Haskell (caso necessário, já existe executável)

#### Usando Cabal:
```bash
cd backend/haskell
cabal build
cabal install --installdir=. --install-method=copy
cd ../..
```

#### Usando Stack:
```bash
cd backend/haskell
stack build
stack install --local-bin-path=.
cd ../..
```

**Nota**: O executável compilado deve estar localizado em `backend/haskell/simulator-backend`

## 🚀 Como Usar

### Executar com Backend Python (padrão)
```bash
python run.py
```

### Executar com Backend Haskell
```bash
python run.py --use-haskell
# ou
python run.py -hs
```

### Opções de linha de comando
```bash
python run.py -h    # Exibe ajuda
python run.py -hs   # Usa backend Haskell
```

## 🎮 Controles da Interface

*[Adicione aqui os controles específicos da sua interface, como:]*
- **Mouse**: Interação com objetos
- **Teclas de seta**: Navegação
- **Espaço**: Pausar/Continuar simulação
- **R**: Reiniciar simulação
- **ESC**: Sair