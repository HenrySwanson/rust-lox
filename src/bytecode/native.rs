use super::value::{Value, NativeFnType};

pub fn get_natives() -> &'static [(&'static str, usize, NativeFnType)] {
	&[("clock", 0, clock)]
}

fn clock(_args: &[Value]) -> Result<Value, String> {
	use std::time::{SystemTime, UNIX_EPOCH};
    let duration = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("SystemTime before UNIX EPOCH!");

    Ok(Value::Number(duration.as_secs() as i64))
}