require("rlang", quietly=TRUE) # permite manejar entornos, expresiones y evaluaciones no estándar en R. El argumento quietly=T evita mjes si la librería ya está cargada.

# workflow que voy a correr
<<<<<<< HEAD:src/workflows/903_run_workflow.r
PARAM <- "src/workflows/918_workflow_base_f202108.r"
=======
PARAM <- "src/workflows/z918_workflow_base_f202108.r" # Especifica la ruta del archivo R que contiene el script del workflow que se va a ejecutar. Le agrego un CZ al final para chequear si se crea un nuevo script CZ.r
>>>>>>> 05756b156181349739e2afd357b9aca7e23ea065:src/workflows/z903_run_workflow.r

envg <- env() # Crea un entorno personalizado donde se almacenarán variables relacionadas con la ejecución del workflow.

envg$EXPENV <- list()
envg$EXPENV$repo_dir <- "~/dmeyf2024/" # Define la ubicación del directorio raíz del repositorio donde está alojado el código del proyecto

#------------------------------------------------------------------------------

# La función definida a continuación:
#### 1° crea un directorio temporal ~/tmp para ejecutar el script sin modificar otros archivos del proyecto y se posiciona como directorio (setwd)
#### 2° crea el script de shell

#Configura un entorno y directorio temporal para ejecutar un workflow.
#Genera un script de shell (run.sh) que corre el script R del workflow (PARAM).
#Otorga permisos al script y lo ejecuta en un entorno limpio con prioridad baja.
#Gestiona todo el proceso automáticamente, incluyendo activación y desactivación de un entorno virtual si es necesario.

Notas importantes

Virtualenv: La inclusión de source /home/$USER/.venv/bin/activate sugiere que el workflow podría necesitar ejecutar código en Python desde un entorno virtual.
Prioridad baja: Usar nice -n 15 asegura que la ejecución no sobrecargue el sistema si hay otros procesos importantes.
Rscript --vanilla: Garantiza que el script R se ejecute en un entorno limpio, sin cargar configuraciones previas que puedan afectar el resultado.



correr_workflow <- function( wf_scriptname )
{
  dir.create( "~/tmp", showWarnings = FALSE)
  setwd("~/tmp" )

  # creo el script que corre el experimento
  comando <- paste0( 
      "#!/bin/bash\n", # define que se usará Bash para interpretar el script.
      "source /home/$USER/.venv/bin/activate\n", # Activa un entorno virtual de Python si es necesario (aunque no parece usarse directamente en este script).
      "nice -n 15 Rscript --vanilla ", # Establece una baja prioridad para el proceso (valor 15) para no consumir demasiados recursos del sistema.  Ejecuta el script de R (wf_scriptname) con el entorno más limpio posible (--vanilla).
      envg$EXPENV$repo_dir,
      wf_scriptname,
      "   ",
      wf_scriptname,
     "\n",
     "deactivate\n" # Desactiva el entorno virtual al finalizar.
    )
  cat( comando, file="run.sh" )

  Sys.chmod( "run.sh", mode = "744", use_umask = TRUE) # Otorga permisos para:  El propietario: leer, escribir y ejecutar. Otros usuarios: solo leer y ejecutar.

  system( "./run.sh" ) # ejecuta el script
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# aqui efectivamente llamo al workflow
correr_workflow( PARAM )
