# Protocolo de Comunicação Backend-Frontend

Este documento descreve o protocolo JSON usado para comunicação entre a interface (frontend) e o simulador (backend).

## Visão Geral

A comunicação acontece via stdin/stdout usando mensagens JSON, uma por linha. O backend é iniciado como um subprocess e mantém comunicação bidirecional com o frontend.

## Fluxo de Comunicação

1. **Inicialização**: Backend envia estado inicial automaticamente
2. **Loop Principal**: Frontend envia comandos, backend responde
3. **Finalização**: Frontend termina o processo backend

## Comandos do Frontend para Backend

### 1. Update (Atualizar Simulação)
```json
{
    "command": "update",
    "dt": 0.016,
    "paused": false
}
```

**Parâmetros:**
- `dt`: Delta time em segundos (float)
- `paused`: Se a simulação está pausada (boolean)

### 2. Set Time Scale (Alterar Velocidade)
```json
{
    "command": "set_time_scale",
    "multiplier": 2.0
}
```

**Parâmetros:**
- `multiplier`: Multiplicador de velocidade (float)

### 3. Add Body (Adicionar Corpo)
```json
{
    "command": "add_body",
    "body": {
        "name": "Planeta 1",
        "mass": 1e25,
        "position": [1.5e11, 0],
        "velocity": [0, 30000],
        "radius": 10,
        "color": [255, 100, 50]
    }
}
```

**Parâmetros do corpo:**
- `name`: Nome do corpo (string)
- `mass`: Massa em kg (float)
- `position`: Posição [x, y] em metros (array de floats)
- `velocity`: Velocidade [vx, vy] em m/s (array de floats)
- `radius`: Raio visual em pixels (int)
- `color`: Cor RGB (array de ints 0-255)

### 4. Reset (Reiniciar Simulação)
```json
{
    "command": "reset"
}
```

### 5. Get State (Obter Estado)
```json
{
    "command": "get_state"
}
```

## Respostas do Backend para Frontend

### Estado do Sistema
```json
{
    "bodies": [
        {
            "name": "Sol",
            "position": [0, 0],
            "radius": 40,
            "color": [255, 220, 0]
        },
        {
            "name": "Terra",
            "position": [1.496e11, 0],
            "radius": 10,
            "color": [100, 149, 237]
        }
    ],
    "stats": {
        "total_bodies": 9,
        "total_mass": 1.989e30,
        "total_kinetic_energy": 1.2e33,
        "bodies_removed": 0,
        "collisions_detected": 0,
        "ejections_detected": 0
    }
}
```

### Confirmação de Comando
```json
{
    "status": "ok"
}
```

### Erro
```json
{
    "error": "Descrição do erro"
}
```

## Dados Trafegados

### Frontend → Backend
- **dt**: Delta time para atualização física
- **paused**: Estado de pausa
- **multiplier**: Multiplicador de velocidade de simulação
- **body data**: Dados completos de novos corpos

### Backend → Frontend
- **bodies**: Lista de corpos com posição, raio e cor para renderização
- **stats**: Estatísticas do sistema para interface
- **status/error**: Confirmações e erros

## Considerações de Implementação

### Performance
- Apenas dados necessários para renderização são enviados (posição, raio, cor)
- Dados físicos completos (massa, velocidade) ficam no backend
- Comunicação assíncrona para evitar bloqueios

### Extensibilidade
- Backend em Haskell deve implementar o mesmo protocolo JSON
- Novos comandos podem ser adicionados mantendo compatibilidade
- Dados são primitivos (não objetos) para facilitar serialização

### Tratamento de Erros
- Timeout para respostas do backend
- Verificação de processo vivo
- Mensagens de erro padronizadas

## Exemplo de Implementação em Haskell

Para implementar um backend em Haskell, você precisará:

1. **Parser JSON** para comandos de entrada
2. **Encoder JSON** para respostas
3. **Loop principal** lendo stdin e escrevendo stdout
4. **Simulador físico** equivalente ao Python

```haskell
-- Estrutura básica
main :: IO ()
main = do
    initialState <- getInitialState
    putStrLn $ encode initialState
    processCommands simulator

processCommands :: Simulator -> IO ()
processCommands sim = do
    line <- getLine
    case decode line of
        Just command -> do
            (newSim, response) <- handleCommand sim command
            putStrLn $ encode response
            processCommands newSim
        Nothing -> processCommands sim
```

## Testando a Comunicação

Para testar manualmente o protocolo:

```bash
# Iniciar backend
python simulator_backend.py

# Enviar comandos (digite no terminal)
{"command": "get_state"}
{"command": "update", "dt": 0.016, "paused": false}
{"command": "set_time_scale", "multiplier": 2.0}
```