use nu_plugin::{EngineInterface, EvaluatedCall, PluginCommand};
use nu_protocol::{
    Category, CommandCompletion, Completion, Example, Flag, LabeledError, PipelineData,
    PositionalArg, Signature, SyntaxShape,
};

use crate::ExamplePlugin;

/// `<list> | example sum`
pub struct ArgCompletion;

impl PluginCommand for ArgCompletion {
    type Plugin = ExamplePlugin;

    fn name(&self) -> &str {
        "example arg-completion"
    }

    fn description(&self) -> &str {
        "It's a demo for arg completion, you can try to type `example arg-completion -f <tab>`to see what it returns"
    }

    fn signature(&self) -> Signature {
        Signature::build(self.name())
            .param(
                PositionalArg::new("first", SyntaxShape::String)
                    .desc("required first")
                    .completion(Completion::Builtin(Box::new(CommandCompletion {
                        internals: false,
                        externals: true,
                        quote: false,
                    })))
                    .required(),
            )
            .param(
                PositionalArg::new("second", SyntaxShape::String)
                    .desc("optional second")
                    .optional(),
            )
            .param(
                Flag::new("future-timestamp")
                    .short('f')
                    .desc("example flag which support auto completion")
                    .completion(Completion::Plugin("example complete arg-completion".into()))
                    .arg(SyntaxShape::Int),
            )
            .category(Category::Experimental)
    }

    fn search_terms(&self) -> Vec<&str> {
        vec!["example"]
    }

    fn examples(&self) -> Vec<Example<'_>> {
        vec![]
    }

    fn run(
        &self,
        _plugin: &ExamplePlugin,
        _engine: &EngineInterface,
        _call: &EvaluatedCall,
        _input: PipelineData,
    ) -> Result<PipelineData, LabeledError> {
        Ok(PipelineData::empty())
    }
}
