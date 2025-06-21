#!/usr/bin/env python3
"""
Ponto de entrada principal do simulador
Permite escolher entre diferentes backends
"""

import argparse
import os
from typing import Dict, Any

from frontend.window import MainWindow


def parse_args() -> Dict[str, Any]:
    """Parseia os argumentos da linha de comando"""
    parser = argparse.ArgumentParser(
        description="Simulador de sistema solar",
        usage="python run.py [-hs]",
        epilog="Use -hs para usar o backend Haskell (padrão: Python)",
        add_help=True,
        formatter_class=argparse.RawTextHelpFormatter
    )
    parser.add_argument(
        '-hs', '--use-haskell', action='store_true',
        help="Usa Haskell como backend (padrão: Python)"
    )
    return vars(parser.parse_args())


def main(args: Dict[str, Any]) -> None:
    use_haskell = args.get('use_haskell', False)

    # script_dir = os.path.dirname(os.path.abspath(__file__))
    # if use_haskell:
    #     backend_path = os.path.join(script_dir, 'backend/haskell/simulator-backend')
    # else:
    #     backend_path = os.path.join(script_dir, 'backend/python/simulator_backend.py')
    if use_haskell:
        backend_path = 'backend/haskell/simulator-backend'
    else:
        backend_path = 'backend/python/simulator_backend.py'

    # Verificar se o caminho do backend existe
    if not os.path.exists(backend_path) or not os.path.isfile(backend_path):
        print(f"Erro: Caminho do backend '{backend_path}' não encontrado.")
        return
    
    # Criar e executar a janela principal
    window = MainWindow(backend_path)
    print("Iniciando o simulador...")
    window.run()


if __name__ == "__main__":
    args = parse_args()
    main(args)