#!/usr/bin/env python3
"""
Ponto de entrada principal do simulador
Permite escolher entre diferentes backends
"""

import sys
import os
from ui.main_window import MainWindow

def main():
    # Configurar caminho do backend
    backend_path = "python simulator_backend.py"
    
    # Verificar argumentos da linha de comando
    if len(sys.argv) > 1:
        if sys.argv[1] == "--help" or sys.argv[1] == "-h":
            print("Uso: python main.py [backend_path]")
            print()
            print("Exemplos:")
            print("  python main.py                           # Usa backend Python padrão")
            print("  python main.py 'python simulator_backend.py'")
            print("  python main.py './haskell_simulator'     # Para futuro backend Haskell")
            return
        else:
            backend_path = sys.argv[1]
    
    # Verificar se o arquivo do backend existe (para backends Python)
    if backend_path.startswith("python") and "simulator_backend.py" in backend_path:
        if not os.path.exists("simulator_backend.py"):
            print("Erro: simulator_backend.py não encontrado!")
            print("Certifique-se de que o arquivo está no diretório atual.")
            return
    
    print(f"Iniciando simulador com backend: {backend_path}")
    
    # Criar e executar a janela principal
    window = MainWindow(backend_path=backend_path)
    window.run()

if __name__ == "__main__":
    main()