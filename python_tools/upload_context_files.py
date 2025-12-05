#!/usr/bin/env python3
"""
Script para subir automáticamente todos los archivos de la carpeta FOR_CONTEXT
al endpoint RAG de upload del sistema AI Core API.

Uso:
    python upload_context_files.py [--url URL] [--folder FOLDER] [--context CONTEXT]

Ejemplos:
    python upload_context_files.py
    python upload_context_files.py --url http://localhost:4000
    python upload_context_files.py --folder ./FOR_CONTEXT --context PLIEGOS_DESARROLLO
"""

import os
import sys
import argparse
import requests
import time
import urllib3
from pathlib import Path
from typing import List, Dict, Optional

# Deshabilitar advertencias SSL
urllib3.disable_warnings(urllib3.exceptions.InsecureRequestWarning)

class ContextUploader:
    def __init__(self, base_url: str = "https://generalitat-de-catalunya-dev-environment-auo8uoen-dev-a5eded457.cfapps.eu10-005.hana.ondemand.com", bearer_token: str = None):
        self.base_url = base_url.rstrip('/')
        self.upload_endpoint = f"{self.base_url}/api/rag/upload"
        self.session = requests.Session()
        
        # Configurar autenticación Bearer token
        if bearer_token:
            self.session.headers.update({
                'Authorization': f'Bearer {bearer_token}'
            })
        
        # Deshabilitar verificación SSL para Cloud Foundry
        self.session.verify = False
        
        # Tipos de archivo soportados
        self.supported_extensions = {
            '.txt', '.docx', '.md', '.json', '.csv', '.pdf', '.xlsx', '.xls'
        }
        
        # Estadísticas
        self.stats = {
            'total_files': 0,
            'uploaded': 0,
            'skipped': 0,
            'errors': 0,
            'start_time': None,
            'end_time': None
        }

    def find_files(self, folder_path: str) -> List[Path]:
        """Encuentra todos los archivos soportados en la carpeta."""
        folder = Path(folder_path)
        
        if not folder.exists():
            raise FileNotFoundError(f"La carpeta {folder_path} no existe")
        
        if not folder.is_dir():
            raise NotADirectoryError(f"{folder_path} no es una carpeta")
        
        files = []
        for file_path in folder.rglob('*'):
            if file_path.is_file() and file_path.suffix.lower() in self.supported_extensions:
                files.append(file_path)
        
        return sorted(files)

    def determine_context(self, file_path: Path, default_context: str) -> str:
        """Determina el contexto basado en la nueva nomenclatura de pliegos."""
        filename = file_path.name.lower()
        
        # Detectar plantillas (formato: pliego_{tipo}_PLANTILLA[_TAGS])
        if 'plantilla_tags' in filename or '_tags' in filename:
            return 'PLANTILLAS_TAGS'
        elif 'plantilla' in filename:
            return 'PLANTILLAS_BASE'
        
        # Detectar pliegos con ID (formato: CT#######_plec_{tipo}_{modalidad}[_{categoria}])
        if filename.startswith('ct') and '_plec_' in filename:
            if 'validacion' in filename or 'validate' in filename:
                return 'DOCUMENTOS_VALIDACION'
            else:
                return 'PLIEGOS_TERMINADOS_ENTRENAMIENTO'
        
        # Usar contexto por defecto para otros casos
        return default_context

    def upload_file(self, file_path: Path, context: str, uploaded_by: str = "script_auto") -> Dict:
        """Sube un archivo individual al endpoint."""
        try:
            # Obtener tamaño del archivo
            file_size = file_path.stat().st_size
            size_mb = file_size / (1024 * 1024)
            
            print(f"Subiendo: {file_path.name} -> Contexto: {context}")
            if size_mb > 5:  # Si es mayor a 5MB
                print(f"   Archivo grande ({size_mb:.1f}MB) - Esto puede tardar varios minutos...")
            
            with open(file_path, 'rb') as file:
                files = {'document': (file_path.name, file, self.get_mime_type(file_path))}
                data = {
                    'contextId': context,
                    'uploadedBy': uploaded_by,
                    'tags': f"auto_upload,{context.lower()},{file_path.suffix[1:]}"
                }
                
                response = self.session.post(
                    self.upload_endpoint,
                    files=files,
                    data=data,
                    timeout=10
                )
                
                if response.status_code == 200:
                    result = response.json()
                    print(f"   Exito: {result.get('message', 'Subido correctamente')}")
                    self.stats['uploaded'] += 1
                    return {'success': True, 'response': result}
                else:
                    error_msg = f"HTTP {response.status_code}: {response.text[:200]}"
                    print(f"   Error: {error_msg}")
                    self.stats['errors'] += 1
                    return {'success': False, 'error': error_msg}
                    
        except Exception as e:
            error_msg = f"Excepción: {str(e)}"
            print(f"   Error: {error_msg}")
            self.stats['errors'] += 1
            return {'success': False, 'error': error_msg}

    def get_mime_type(self, file_path: Path) -> str:
        """Obtiene el tipo MIME basado en la extensión."""
        mime_types = {
            '.txt': 'text/plain',
            '.md': 'text/markdown',
            '.json': 'application/json',
            '.csv': 'text/csv',
            '.pdf': 'application/pdf',
            '.docx': 'application/vnd.openxmlformats-officedocument.wordprocessingml.document',
            '.xlsx': 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
            '.xls': 'application/vnd.ms-excel'
        }
        return mime_types.get(file_path.suffix.lower(), 'application/octet-stream')

    def test_connection(self) -> bool:
        """Prueba la conexión con el servidor."""
        try:
            health_url = f"{self.base_url}/health"
            response = self.session.get(health_url, timeout=5)
            
            print(health_url)
            if response.status_code == 200:
                print(f"Conexion exitosa con {self.base_url}")
                return True
            else:
                print(f"Error de conexion: HTTP {response.status_code}")
                return False
                
        except Exception as e:
            print(f"Error de conexion: {str(e)}")
            return False

    def upload_all(self, folder_path: str, default_context: str = "default", 
                   uploaded_by: str = "script_auto", delay: float = 1.0) -> Dict:
        """Sube todos los archivos de la carpeta."""
        
        print(f"Iniciando subida masiva desde: {folder_path}")
        print(f"Contexto por defecto: {default_context}")
        print(f"Subido por: {uploaded_by}")
        print("=" * 60)
        
        self.stats['start_time'] = time.time()
        
        # Verificar conexión
        if not self.test_connection():
            return {'success': False, 'error': 'No se pudo conectar al servidor'}
        
        # Encontrar archivos
        try:
            files = self.find_files(folder_path)
            self.stats['total_files'] = len(files)
            
            if not files:
                print("ADVERTENCIA: No se encontraron archivos soportados en la carpeta")
                return {'success': True, 'message': 'No hay archivos para subir'}
            
            print(f"Encontrados {len(files)} archivos soportados")
            print()
            
        except Exception as e:
            return {'success': False, 'error': f'Error accediendo a la carpeta: {str(e)}'}
        
        # Subir archivos
        results = []
        for i, file_path in enumerate(files, 1):
            print(f"[{i}/{len(files)}] ", end="")
            
            # Usar siempre el contexto especificado por el usuario
            result = self.upload_file(file_path, default_context, uploaded_by)
            results.append({
                'file': str(file_path),
                'context': default_context,
                'result': result
            })
            
            # Pausa entre subidas
            if delay > 0 and i < len(files):
                time.sleep(delay)
            
            print()
        
        self.stats['end_time'] = time.time()
        return {'success': True, 'results': results}

    def print_summary(self):
        """Imprime resumen de la operación."""
        if self.stats['start_time'] and self.stats['end_time']:
            duration = self.stats['end_time'] - self.stats['start_time']
            print("=" * 60)
            print("RESUMEN DE SUBIDA")
            print("=" * 60)
            print(f"Total archivos encontrados: {self.stats['total_files']}")
            print(f"Subidos exitosamente: {self.stats['uploaded']}")
            print(f"Omitidos: {self.stats['skipped']}")
            print(f"Errores: {self.stats['errors']}")
            print(f"Tiempo total: {duration:.1f} segundos")
            
            if self.stats['uploaded'] > 0:
                avg_time = duration / self.stats['uploaded']
                print(f"Tiempo promedio por archivo: {avg_time:.1f} segundos")
            
            success_rate = (self.stats['uploaded'] / self.stats['total_files'] * 100) if self.stats['total_files'] > 0 else 0
            print(f"Tasa de exito: {success_rate:.1f}%")


def main():
    parser = argparse.ArgumentParser(
        description="Sube archivos de contexto al sistema RAG automáticamente"
    )
    
    parser.add_argument(
        '--url', 
        default='https://generalitat-de-catalunya-dev-environment-auo8uoen-dev-a5eded457.cfapps.eu10-005.hana.ondemand.com',
        help='URL base del servidor (default: Cloud Foundry)'
    )
    
    parser.add_argument(
        '--folder',
        default='./FOR_CONTEXT',
        help='Carpeta con archivos a subir (default: ./FOR_CONTEXT)'
    )
    
    parser.add_argument(
        '--context',
        default='PLIEGOS_DESARROLLO',
        help='Contexto por defecto (default: PLIEGOS_DESARROLLO)'
    )
    
    parser.add_argument(
        '--uploaded-by',
        default='script_auto',
        help='Identificador de quien sube (default: script_auto)'
    )
    
    parser.add_argument(
        '--delay',
        type=float,
        default=1.0,
        help='Pausa entre subidas en segundos (default: 1.0)'
    )
    
    parser.add_argument(
        '--bearer-token',
        type=str,
        help='Bearer token para autenticación'
    )
    
    parser.add_argument(
        '--dry-run',
        action='store_true',
        help='Solo mostrar qué archivos se subirían sin subirlos'
    )
    
    args = parser.parse_args()
    
    # Solicitar Bearer token si no se proporcionó
    bearer_token = args.bearer_token
    if not bearer_token and not args.dry_run:
        print("Autenticacion requerida para Cloud Foundry")
        bearer_token = input("Introduce el Bearer token: ").strip()
        if not bearer_token:
            print("ERROR: Bearer token requerido para subir archivos")
            sys.exit(1)
    
    # Solicitar contextId
    context_id = args.context
    print(f"Contexto actual: {context_id}")
    new_context = input("Introduce nuevo contextId (Enter para mantener actual): ").strip()
    if new_context:
        context_id = new_context
        print(f"Usando contexto: {context_id}")
    else:
        print(f"Manteniendo contexto: {context_id}")
    print()
    
    # Crear uploader
    uploader = ContextUploader(args.url, bearer_token)
    
    if args.dry_run:
        print("MODO DRY-RUN - Solo mostrando archivos encontrados")
        try:
            files = uploader.find_files(args.folder)
            print(f"Carpeta: {args.folder}")
            print(f"Archivos encontrados: {len(files)}")
            print()
            
            for i, file_path in enumerate(files, 1):
                print(f"{i:2d}. {file_path.name} -> {context_id}")
                
        except Exception as e:
            print(f"ERROR: {e}")
            sys.exit(1)
    else:
        # Subida real
        try:
            result = uploader.upload_all(
                args.folder, 
                context_id, 
                args.uploaded_by,
                args.delay
            )
            
            uploader.print_summary()
            
            if not result['success']:
                print(f"Error general: {result.get('error', 'Error desconocido')}")
                sys.exit(1)
                
        except KeyboardInterrupt:
            print("\nSubida cancelada por el usuario")
            uploader.print_summary()
            sys.exit(1)
        except Exception as e:
            print(f"Error inesperado: {e}")
            sys.exit(1)

if __name__ == "__main__":
    main()
