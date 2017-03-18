# HOpenCV-0.5.0.1
Versión actualizada de la libreria HOpenCV. Binding para haskell de OpenCV

HOpenCV es un binding de OpenCV en C++ por medio de Foreing Function Interface en
Haskell.

# Documentación

Dentro de la carpeta doc/ podrán encontrar la documentación del proyecto como de la librería generada por haddock.

# Instalación

La librería tiene las siguientes dependencias de instalación:

base; hsc2hs; vector-space; allocated-processor; haskell-platform y opencv3.x

Las cuales se las puede instalar ejecutando el script "install.sh".

Para la compilación se desarrolló un Makefile que compila todos los archivos con sus
respectivas banderas. A su vez se desarrollaron algunos ejemplos que muestran la forma de
utilización de la librería.

Para compilar utilizamos el siguiente comando:

ghc –make file.hs HOpenCV/CV/HOpenCV_wrap.cpp ‘pkg-config opencv –cflags –libs‘ -o file

Dentro de la carpeta "examples" se encuentra los ejemplos.


# Repositorio

En la carpeta lib/ se encuentran las versiones anteriores de HOpenCV y CV-Combinator.

El repositorio actual es:
https://github.com/juanmab37/HOpenCV-0.5.0.1

# Autor

Baruffaldi Juan Manuel

baruffaldi.jm (at) gmail.com
