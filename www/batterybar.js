function testArrayKeys(arr1, arr2) {
  for (let i = 0; i < arr1.length; i++) {
    let key = arr1[i];
    
    if (!arr2.hasOwnProperty(key)) {
      return false;
    }
  }
  
  return true;
}

function styleBatteryBar(data, colormap) {
  if (!Array.isArray(data)) {
    console.warn("An array is required");
    return "";
  }
  
  if(!testArrayKeys(data, colormap)) {
    console.warn("Each value of data must be a key in colormap")
    return "";
  }

  const n_data = data.length;
  const margin = 1;
  const size = 100 / n_data;

  let output = "linear-gradient(";
  let boxes = ["to right"];

  for (let i = 0; i < data.length; i++) {
    const col = colormap[data[i]];
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

function sampleWithReplacement(arr, n) {
  let result = [];
  
  for (let i = 0; i < n; i++) {
    let randomIndex = Math.floor(Math.random() * arr.length);
    result.push(arr[randomIndex]);
  }
  
  return result;
}
