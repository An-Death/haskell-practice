data Triple a = Tr a a a deriving (Eq,Show)

instance Functor Triple where
    fmap f (Tr a1 a2 a3) = Tr (f a1) (f a2) (f a3)

  
instance Applicative Triple where
    pure x = Tr x x x
    (Tr f1 f2 f3) <*> (Tr a1 a2 a3) = Tr (f1 a1) (f2 a2) (f3 a3)


newtype ZipList a = ZipList {getZipList :: [a]}
    deriving Show

instance Functor ZipList where
    fmap f (ZipList xs) = ZipList (map f xs)

instance Applicative ZipList where
    pure x = ZipList (repeat x)
    ZipList gs <*> ZipList xs = ZipList (zipWith ($) gs xs)

(>$<) :: (a -> b) -> [a] -> [b]
fn >$< arg = getZipList $ fn <$> ZipList arg

(>*<) :: [(a -> b)] -> [a] -> [b]
fn >*< arg = getZipList $ ZipList fn <*> ZipList arg

divideList' :: (Show a, Fractional a) => [a] -> (String,a)
divideList' []     = ("1.0", 1.0)
divideList' (x:xs) = (/) <$> ("<-" ++ show x ++ "/", x) <*> (divideList' xs)



data Stat = Stat {total_gross::Double, taxed_gross::Double, total_net::Double, total_tax:: Double, social_tax:: Double, income_tax::Double, month_net::Double, month_social_tax::Double, month_income_tax::Double, tax_percentage::Double} 
    deriving Show

calculate_tax is_first_five_years gross month = 
    Stat {
        total_gross = total_gross',
        taxed_gross = taxed_gross,
        total_net   = total_gross' - total_tax,
        total_tax   = total_tax,
        social_tax  = social_tax,
        income_tax  = income_tax,
        month_net   = (total_gross' - total_tax) / month,

        month_social_tax = social_tax / month,
        month_income_tax = income_tax / month,
        tax_percentage   = percents total_gross' total_tax
    }
    where 
        total_gross' = total_grossF gross month
        taxed_gross = tax_all_sum total_gross'

        social_tax  = social_insurance_tax total_gross'
        income_tax  = tax_rate taxed_gross
        total_tax   = social_tax + income_tax

        tax_all_sum total_gross = total_gross * (1.0-social_insurance_tax_rate) * (if is_first_five_years then 0.8 else 1.0)

total_grossF gross month = gross * month
percents x y = (1/(x/y))*100
social_insurance_tax total_gross = total_gross * social_insurance_tax_rate
social_insurance_tax_rate = 0.078
tax_rate allSum         
    | allSum <= 19500 = 0
    | allSum <= 28000 = (allSum-19501)*0.2
    | allSum <= 36300 = (allSum-28001)*0.25 + tax_rate 28000 
    | allSum <= 60000 = (allSum - 36301)*0.3 + tax_rate 36300
    | allSum >  60000 = (allSum - 60001)*0.35 + tax_rate 60000


evklide x y 
        | (x <=0 ) || (y <= 0) = error ("One of values: " ++ show x ++ " " ++ show y ++ " must be GT then 0")
        | x == y = x
        | x > y = evklide (x-y) y
        | x < y = evklide x (y-x)
        | otherwise = error (show x ++ " and " ++ show y ++ " havent delim")

