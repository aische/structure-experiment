# "Structure" (version 2)


	class FunctorS (t :: (* -> *) -> *) where
	  mapS :: Forall (Func m n) -> t m -> t n

	class ApplicativeS (t1 :: (* -> *) -> *) (t2 :: (* -> *) -> *) where
	  appS :: t1 (Func m n) -> t2 m -> t2 n

	class TraversableS (t :: (* -> *) -> *) where
	  seqS :: Applicative m => t (Comp m n) -> m (t n)

	class ParseS (t1 :: (* -> *) -> *) (t2 :: (* -> *) -> *) where
	  parseS :: Alternative m => t1 m -> m (t2 Identity)


	data Func m n a = Func { unFunc :: m a -> n a }

	data Comp m n a = Comp { unComp :: m (n a) }

	data Forall m = Forall { unForall :: forall a . m a }


	data Product (a :: (* -> *) -> *) (b :: (* -> *) -> *) (m :: * -> *)
	  = Product (a m) (b m)

	data Sum (a :: (* -> *) -> *) (b :: (* -> *) -> *) (m :: * -> *)
	  = LeftS (a m)
	  | RightS (b m)


	readS :: (TraversableS t, FunctorS t, Monad m) => t (ReaderT i m) -> Reader i (t m)

	runStateS :: (FunctorS t, TraversableS t, Monad m) => t (StateT acc m) -> StateT acc m (t Identity)

	toIdentity :: (Applicative m, FunctorS t, TraversableS t) => t m -> m (t Identity)

	toEmpty :: (Alternative n, FunctorS t) => t (m :: * -> *) -> t n

	zipWithS :: (FunctorS t1, ApplicativeS t1 t2) => Forall (Func m (Func n o)) -> t1 m -> t2 n -> t2 o

	mapConstS :: FunctorS t => (a -> b) -> t (Const a) -> t (Const b)

	duplicateS :: (Applicative m, FunctorS t) => t m -> t (Comp m m)

	extendS :: (Functor m, Applicative n, FunctorS t) => t m -> t (Comp m n)

	composeS :: (FunctorS t1, ApplicativeS t1 t2) => t1 (Func m n) -> t2 (Func n o) -> t2 (Func m o)

	type FoldF m state = Func Identity (StateT state m)

	foldS :: (Monad m, ApplicativeS t1 t2, FunctorS t2, TraversableS t2) => t1 (FoldF m acc) -> t2 Identity -> StateT acc m (t2 Identity)

