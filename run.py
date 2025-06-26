#!/usr/bin/env python3
"""
Ponto de entrada principal do simulador
Permite escolher entre diferentes backends
"""

import argparse
import os
import sys
from pathlib import Path
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

    base_dir = Path(__file__).parent

    if use_haskell:
        executable_name_with_parent = ("windows/simulator-backend.exe" if sys.platform.startswith('win') 
                                       else "linux/simulator-backend")
        backend_path: Path = base_dir / 'backend' / 'haskell' / executable_name_with_parent
    else:
        backend_path = base_dir / 'backend' / 'python' / 'simulator_backend.py'

    backend_path = backend_path.resolve()

    # Verificar se o caminho do backend existe
    if not os.path.exists(backend_path) or not os.path.isfile(backend_path):
        print(f"Erro: Caminho do backend '{backend_path}' não encontrado.")
        return
    
    # Criar e executar a janela principal
    print("Iniciando o simulador...")
    window = MainWindow(backend_path.__str__())
    window.run()


if __name__ == "__main__":
    args = parse_args()
    main(args)