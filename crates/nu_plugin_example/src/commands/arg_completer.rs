use nu_plugin::{EngineInterface, EvaluatedCall, PluginCommand};
use nu_protocol::{
    Category, Example, LabeledError, PipelineData, Signature, SyntaxShape, Type, Value,
};

use crate::ExamplePlugin;
use std::time::{SystemTime, UNIX_EPOCH};

/// Demo completer command for `example arg-completion -f`
pub struct ArgCompleter;

impl PluginCommand for ArgCompleter {
    type Plugin = ExamplePlugin;

    fn name(&self) -> &str {
        "example complete arg-completion"
    }

    fn description(&self) -> &str {
        "Demo argument completer command."
    }

    fn signature(&self) -> Signature {
        Signature::build(self.name())
            .input_output_types(vec![(Type::Any, Type::Any)])
            .optional("input", SyntaxShape::String, "User input")
            .category(Category::Experimental)
    }

    fn search_terms(&self) -> Vec<&str> {
        vec![]
    }

    fn examples(&self) -> Vec<Example<'_>> {
        vec![]
    }

    fn run(
        &self,
        _plugin: &ExamplePlugin,
        _engine: &EngineInterface,
        call: &EvaluatedCall,
        _input: PipelineData,
    ) -> Result<PipelineData, LabeledError> {
        // let's generate it dynamically too
        let start = SystemTime::now();
        let since_the_epoch = start
            .duration_since(UNIX_EPOCH)
            .expect("time should go forward")
            .as_secs();
        let values: Vec<Value> = (since_the_epoch..since_the_epoch + 10)
            .map(|s| Value::string(s.to_string(), call.head))
            .collect();
        Ok(PipelineData::Value(Value::list(values, call.head), None))
    }
}
