package carter.fchains

object ChainExecutor {

  def apply[Out](chain: Chain[Out]): Out = chain match {
    case ChainRoot(provider) =>
      provider.f()
    case ChainStep(lower, transform) =>
      transform.f(apply(lower))
    case split: ChainSplit[_, Out] =>
      split.runAll(split.chains)
  }
}
