import Data.Char (toLower)
import Data.List
import System.IO
import Control.Exception
import Control.DeepSeq (deepseq)

-- Definición del tipo de datos para representar la información de un Articulo
data Articulo = Articulo {
    nombre :: String,
    categoria :: String
} deriving (Show, Read)

-- Funcion para poner la categoria en minusculas
normalizarCategoria :: String -> String
normalizarCategoria = map toLower

-- Función para registrar la entrada de un articulo al inventario
registrarEntrada :: String -> String -> [Articulo] -> [Articulo]
registrarEntrada nombreArticulo categoriaArticulo inventario =
    Articulo nombreArticulo (normalizarCategoria categoriaArticulo) : inventario

-- Función para buscar articulos por categoría en el inventario
buscarPorCategoria :: String -> [Articulo] -> [Articulo]
buscarPorCategoria categoriaArticulo inventario =
    filter (\art -> categoria art == normalizarCategoria categoriaArticulo) inventario

-- Función para listar todos los articulos en el inventario
listarArticulos :: [Articulo] -> IO ()
listarArticulos [] = putStrLn "No hay artículos en el inventario."
listarArticulos inventario = do
    putStrLn "Artículos en el inventario:"
    mapM_ (putStrLn . mostrarArticulo) inventario

-- Funcion para contar articulos por categoria
contarPorCategoria :: String -> [Articulo] -> Int
contarPorCategoria categoriaArticulo inventario =
    length $ buscarPorCategoria categoriaArticulo inventario

-- Funcin para mostrar la información de un artículo como cadena de texto
mostrarArticulo :: Articulo -> String
mostrarArticulo (Articulo nombre categoria) =
    "Articulo {nombre = \"" ++ nombre ++ "\", categoria = \"" ++ categoria ++ "\"}"

-- Función para guardar el inventario en un archivo de texto 
guardarInventario :: [Articulo] -> IO ()
guardarInventario inventario = do
    resultado <- try (withFile "inventario.txt" WriteMode $ \h -> do
        hPutStr h (unlines (map mostrarArticulo inventario))) :: IO (Either IOException ())
    case resultado of
        Left ex -> putStrLn $ "Error al guardar el inventario: " ++ show ex
        Right _ -> do
            putStrLn "Inventario guardado en el archivo inventario.txt."
            hFlush stdout

-- Función para cargar el inventario desde un archivo de texto 
cargarInventario :: IO [Articulo]
cargarInventario = do
    resultado <- try (withFile "inventario.txt" ReadMode $ \h -> do
        contenido <- hGetContents h
        contenido `deepseq` return contenido) :: IO (Either IOException String)
    case resultado of
        Left ex -> do
            putStrLn $ "Error al leer el inventario: " ++ show ex
            return []
        Right contenido -> do
            let lineas = lines contenido
            return (map leerArticulo lineas)
    where
        leerArticulo linea = read linea :: Articulo

-- Función principal del programa
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering  
    inventario <- cargarInventario
    putStrLn "¡Bienvenido al Sistema de Gestión de Inventario!"

    -- Ciclo principal del programa
    cicloPrincipal inventario

-- Función para el ciclo principal del programa
cicloPrincipal :: [Articulo] -> IO ()
cicloPrincipal inventario = do
    putStrLn "\nSeleccione una opcion:"
    putStrLn "1. Registrar entrada de articulo"
    putStrLn "2. Buscar articulos por categoria"
    putStrLn "3. Listar articulos en Inventario"
    putStrLn "4. Contar articulos por categoria"
    putStrLn "5. Salir"
    
    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Ingrese el nombre del articulo:"
            nombreArticulo <- getLine
            putStrLn "Ingrese la categoria del articulo:"
            categoriaArticulo <- getLine
            let inventarioActualizado = registrarEntrada nombreArticulo categoriaArticulo inventario
            putStrLn $ "Articulo " ++ nombreArticulo ++ " registrado en el inventario."
            guardarInventario inventarioActualizado
            cicloPrincipal inventarioActualizado

        "2" -> do
            putStrLn "Ingrese la categoria a buscar:"
            categoriaArticulo <- getLine
            let resultados = buscarPorCategoria categoriaArticulo inventario
            listarArticulos resultados
            cicloPrincipal inventario

        "3" -> do
            listarArticulos inventario
            cicloPrincipal inventario

        "4" -> do
            putStrLn "Ingrese la categoria para contar:"
            categoriaArticulo <- getLine
            let cuenta = contarPorCategoria categoriaArticulo inventario
            putStrLn $ "Hay " ++ show cuenta ++ " articulos en la categoria " ++ categoriaArticulo
            cicloPrincipal inventario

        "5" -> putStrLn "¡Hasta luego!"

        _ -> do
            putStrLn "Opción no válida. Por favor, seleccione una opción válida."
            cicloPrincipal inventario
