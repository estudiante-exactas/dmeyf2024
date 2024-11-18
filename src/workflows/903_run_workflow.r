require("rlang", quietly=TRUE) # permite manejar entornos, expresiones y evaluaciones no est�ndar en R. El argumento quietly=T evita mjes si la librer�a ya est� cargada.

# workflow que voy a correr
<<<<<<< HEAD:src/workflows/903_run_workflow.r
PARAM <- "src/workflows/918_workflow_base_f202108.r"
=======
PARAM <- "src/workflows/z918_workflow_base_f202108.r" # Especifica la ruta del archivo R que contiene el script del workflow que se va a ejecutar. Le agrego un CZ al final para chequear si se crea un nuevo script CZ.r
>>>>>>> 05756b156181349739e2afd357b9aca7e23ea065:src/workflows/z903_run_workflow.r

envg <- env() # Crea un entorno personalizado donde se almacenar�n variables relacionadas con la ejecuci�n del workflow.

envg$EXPENV <- list()
envg$EXPENV$repo_dir <- "~/dmeyf2024/" # Define la ubicaci�n del directorio ra�z del repositorio donde est� alojado el c�digo del proyecto

#------------------------------------------------------------------------------

# La funci�n definida a continuaci�n:
#### 1� crea un directorio temporal ~/tmp para ejecutar el script sin modificar otros archivos del proyecto y se posiciona como directorio (setwd)
#### 2� crea el script de shell

#Configura un entorno y directorio temporal para ejecutar un workflow.
#Genera un script de shell (run.sh) que corre el script R del workflow (PARAM).
#Otorga permisos al script y lo ejecuta en un entorno limpio con prioridad baja.
#Gestiona todo el proceso autom�ticamente, incluyendo activaci�n y desactivaci�n de un entorno virtual si es necesario.

Notas importantes

Virtualenv: La inclusi�n de source /home/$USER/.venv/bin/activate sugiere que el workflow podr�a necesitar ejecutar c�digo en Python desde un entorno virtual.
Prioridad baja: Usar nice -n 15 asegura que la ejecuci�n no sobrecargue el sistema si hay otros procesos importantes.
Rscript --vanilla: Garantiza que el script R se ejecute en un entorno limpio, sin cargar configuraciones previas que puedan afectar el resultado.



correr_workflow <- function( wf_scriptname )
{
  dir.create( "~/tmp", showWarnings = FALSE)
  setwd("~/tmp" )

  # creo el script que corre el experimento
  comando <- paste0( 
      "#!/bin/bash\n", # define que se usar� Bash para interpretar el script.
      "source /home/$USER/.venv/bin/activate\n", # Activa un entorno virtual de Python si es necesario (aunque no parece usarse directamente en este script).
      "nice -n 15 Rscript --vanilla ", # Establece una baja prioridad para el proceso (valor 15) para no consumir demasiados recursos del sistema.  Ejecuta el script de R (wf_scriptname) con el entorno m�s limpio posible (--vanilla).
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
