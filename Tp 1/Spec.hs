import MultiDict
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do

    describe "Ejercicio 3" $ do
        context "con input random" $ do
        it "Devuelve siempre profundidad 2" $ property $ 
            \x -> profundidad (podar 5 3 $ tablas x) == (2 :: Integer)

    describe "ejercicio 6" $ do
        it "Cadena de lista vacia devuelve diccionario vacio" $ do
            (cadena 1 []) `shouldBe` (Nil :: MultiDict () Int)

        context "Con lista random" $ do
            it "La profundidad y el tamaño siempre son iguales" $ property $
                \x -> profundidad (cadena "abc" (x::[Integer])) == tamaño (cadena "abc" (x::[Integer]))
    
    describe "Ejercicio 5.a" $ do
        it "Map de caso base devuelve caso base" $ do
            mapMD (id) (id) Nil `shouldBe` (Nil :: MultiDict () ())

    describe "Ejercicio 5.b" $ do
        it "Filter de caso base devuelve caso base" $ do
            filterMD (const True) Nil `shouldBe` (Nil :: MultiDict () ())
            

    describe "ejercicio 7" $ do
        it "Obtener de una rama valida devuelve el valor" $ do
            obtener [1, 2, 3, 4] (definir [1, 2, 3, 4] 'e' Nil)  `shouldBe` (Just 'e')
        it "Obtener de una rama no valida devuelve nada" $ do
            obtener [1, 2, 3, 4, 5] (definir [1, 2, 3, 4] 'e' Nil)  `shouldBe` Nothing