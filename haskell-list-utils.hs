module Main where

-- 1. ikinci_yok: Verilen listeden ikinci elemanı kaldırır.
--    İlk iki elemanı x ve _ olarak ayırır, ardından x'i koruyup geri kalanı ekleyerek ikinci öğeyi atlar.
ikinci_yok :: [a] -> [a]
ikinci_yok (x:_:xs) = x : xs

-- 2. sırala: Quicksort algoritmasıyla sıralama yapar.
--    Pivot olarak ilk elemanı seçer, ondan küçük veya eşitleri ve büyükleri ayrı ayrı sıralayıp birleştirir.
sırala :: (Ord a) => [a] -> [a]
sırala []     = []
sırala (p:xs) = sırala [x | x <- xs, x <= p]
                ++ [p]
                ++ sırala [x | x <- xs, x > p]

-- 3. sadece_cift: Listeden yalnızca çift sayıları döndürür.
--    "even" fonksiyonunu kullanarak her elemanın çift olup olmadığını kontrol eder.
sadece_cift :: Integral a => [a] -> [a]
sadece_cift xs = [x | x <- xs, even x]

-- 4. min3: Üç sayı arasından en küçüğünü bulur.
--    "min" fonksiyonunu ardışık olarak kullanarak önce x ve y, sonra sonuç ile z karşılaştırması yapar.
min3 :: Ord a => a -> a -> a -> a
min3 x y z = min x (min y z)

-- 5. carp_uc: Listedeki her sayıyı üç ile çarpar.
--    List comprehension ile her x öğesini 3*x biçiminde dönüştürür.
carp_uc :: Num a => [a] -> [a]
carp_uc xs = [3 * x | x <- xs]

-- 6. ikile: Her elemanı kendisiyle ikili tuple halinde tekrarlar.
--    Liste öğelerini (x, x) formatında çiftleyerek yeni liste oluşturur.
ikile :: [a] -> [(a, a)]
ikile xs = [(x, x) | x <- xs]

-- 7. en_buyuk: Listede en büyük elemanı döndürür.
--    Boş listede hata verir; dolu listede "foldr1 max" kullanarak maksimumu belirler.
en_buyuk :: Ord a => [a] -> a
en_buyuk [] = error "en_buyuk: boş listede tanımsız"
en_buyuk xs = foldr1 max xs

-- 8. topla: İki listenin aynı pozisyondaki elemanlarını toplar.
--    "zipWith (+)" fonksiyonu ile karşılıklı toplama yaparak sonuç listesi üretir.
topla :: Num a => [a] -> [a] -> [a]
topla xs ys = zipWith (+) xs ys

-- 9. siniflandir_g: Not değerine göre harf notu ataması yapar (guard kullanımı).
--    90 ve üzeri 'A', 80-89 'B', 70-79 'C', diğerleri 'D'.
siniflandir_g :: (Ord a, Num a) => a -> Char
siniflandir_g n
  | n >= 90           = 'A'
  | n >= 80 && n <= 89 = 'B'
  | n >= 70 && n < 80  = 'C'
  | otherwise          = 'D'

-- 10. carpanlar: Verilen sayının tüm pozitif çarpanlarını döndürür.
--     1'den x/2'ye kadar bölenleri kontrol eder, kalansız bölünenleri listeye ekler.
carpanlar :: Integral a => a -> [a]
carpanlar x = [d | d <- [1 .. (x `div` 2)], x `mod` d == 0]

-- Ana fonksiyon: iki farklı girdi setini test eder ve sonuçları yazdırır
main :: IO ()
main = do
  -- Birinci giriş seti
  putStrLn "-- Girdi Seti 1 --"
  putStrLn "ikinci_yok [3,5,4,2] ->" >> print (ikinci_yok [3,5,4,2])
  putStrLn "sırala [5,4,1,6,8] ->" >> print (sırala [5,4,1,6,8])
  putStrLn "sadece_cift [1,2,3,4,5,6] ->" >> print (sadece_cift [1,2,3,4,5,6])
  putStrLn "min3 4 2 3 ->" >> print (min3 4 2 3)
  putStrLn "carp_uc [3,5,6] ->" >> print (carp_uc [3,5,6])
  putStrLn "ikile [1,2,3] ->" >> print (ikile [1,2,3])
  putStrLn "en_buyuk [1,2,5,4,4] ->" >> print (en_buyuk [1,2,5,4,4])
  putStrLn "topla [3,4,5] [6,8,1] ->" >> print (topla [3,4,5] [6,8,1])
  putStrLn "siniflandir_g 87 ->" >> print (siniflandir_g 87)
  putStrLn "carpanlar 15 ->" >> print (carpanlar 15)

  -- İkinci giriş seti
  putStrLn "\n-- Girdi Seti 2 --"
  putStrLn "ikinci_yok [10,20,30,40] ->" >> print (ikinci_yok [10,20,30,40])
  putStrLn "sırala [9,2,7,4] ->" >> print (sırala [9,2,7,4])
  putStrLn "sadece_cift [10,11,12,13] ->" >> print (sadece_cift [10,11,12,13])
  putStrLn "min3 100 50 75 ->" >> print (min3 100 50 75)
  putStrLn "carp_uc [0,1,2] ->" >> print (carp_uc [0,1,2])
  putStrLn "ikile ['a','b'] ->" >> print (ikile ['a','b'])
  putStrLn "en_buyuk [5,3,8,1] ->" >> print (en_buyuk [5,3,8,1])
  putStrLn "topla [1,1,1] [2,2,2] ->" >> print (topla [1,1,1] [2,2,2])
  putStrLn "siniflandir_g 59 ->" >> print (siniflandir_g 59)
  putStrLn "carpanlar 21 ->" >> print (carpanlar 21)