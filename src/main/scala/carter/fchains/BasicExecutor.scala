package carter.fchains

import cats.Id

object BasicExecutor extends ChainExecutor[Id] {

  def execute[Out](chain: Chain[Out]): Out = chain match {
    case ChainRoot(provider) =>
      provider.f()
    case ChainStep(init, transform) =>
      transform.f(execute(init))
    case split: ChainSplit[_, Out] =>
      split.run(this)
  }
}
