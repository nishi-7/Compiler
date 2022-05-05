package com.github.nishi_7
package mid

trait PipelineProcess[From, To] {
  def process(context: CompileContext[From]): CompileContext[To]
}
