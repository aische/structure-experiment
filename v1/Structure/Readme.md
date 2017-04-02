# "Structure" (version 1)


	class Structure t where
		mapS :: Forall (Func m n) -> t m -> t n
		appS :: t (Func m n) -> t m -> t n
		seqS :: Applicative m => t (Comp m n) -> m (t n)
	

	data Forall m = Forall { unForall :: forall a . m a }
	
	data Func m n a = Func { unFunc :: m a -> n a }
	
	data Comp m n a = Comp { unComp :: m (n a) }

	
	readS :: (Structure t, Monad m) => t (ReaderT i m) -> Reader i (t m)

	runStateS :: (Structure t, Monad m) => t (StateT acc m) -> StateT acc m (t Identity)

	toIdentity :: (Applicative m, Structure t) => t m -> m (t Identity)

	toEmpty :: (Alternative n, Structure t) => t (m :: * -> *) -> t n

	zipWithS :: Structure t => Forall (Func m (Func n o)) -> t m -> t n -> t o

	mapConstS :: Structure t => (a -> b) -> t (Const a) -> t (Const b)

	duplicateS :: (Applicative m, Structure t) => t m -> t (Comp m m)

	extendS :: (Functor m, Applicative n, Structure t) => t m -> t (Comp m n)

	composeS :: Structure t => t (Func m n) -> t (Func n o) -> t (Func m o)

	type FoldF m state = Func Identity (StateT state m)

	foldS :: (Monad m, Structure t) => t (FoldF m acc) -> t Identity -> StateT acc m (t Identity)

