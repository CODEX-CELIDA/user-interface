function styleBatteryBar(data) {
  if (!Array.isArray(data) || !data.every((x) => typeof x === "boolean")) {
    console.warn("a boolean array is required");
    return "";
  }

  const n_data = data.length;
  const margin = 1;
  const size = 100 / n_data;

  let output = "linear-gradient(";
  let boxes = ["to right"];

  for (let i = 0; i < data.length; i++) {
    const col = data[i] ? "#d2f7b7" : "#f7c6b7";
    const start = size * i + margin;
    const end = size * (i + 1) - margin;

    boxes.push(`${col} ${start}% ${end}%`);

    if (i < data.length - 1) {
      boxes.push(`white ${end}% ${end + 2 * margin}%`);
    }
  }

  output += boxes.join(", ") + ")";
  return output;
}

function randomBooleanArray(length) {
  let boolArray = [];

  for (let i = 0; i < length; i++) {
    boolArray.push(Math.random() < 0.5);
  }

  return boolArray;
}