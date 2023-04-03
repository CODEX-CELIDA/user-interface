let docStyle = getComputedStyle(document.documentElement);
let colormap = {
   '1': docStyle.getPropertyValue('--population-intervention-color').trim(),
   '0': docStyle.getPropertyValue('--population-color').trim(),
  '-1': docStyle.getPropertyValue('--none-color').trim()
};


function addDataAttributes(row, data, dataIndex) {
  $(row).find('td').each(function(colIndex) {
      
    const cellData = data[colIndex];
    
    if (!Array.isArray(cellData)) {
      return;
    }
    
    $(this).text((parseFloat(cellData[0])*100).toFixed(0) + '%');
    $(this).attr("data-percentage", cellData[0]);
    $(this).attr('data-days', cellData[1]);
    $(this).attr('data-comment', cellData[2].toLowerCase());
  });
}

function addBatteryBarBackground(row, data, dataIndex) {
  
  $(row).find('td[data-days]').each(function() {
    var days = $(this).attr('data-days')
    var days_arr = days.split('').map(str => parseInt(str) - 1);
    
    $(this).css('border', 'solid black 1px');
    $(this).css('background', styleBatteryBar(days_arr, colormap));
    $(this).css('background-repeat','no-repeat');
    $(this).css('background-position','center');
    $(this).css('background-size','98% 88%')
  });
}

function addSpeechBubble(row, data, dataIndex) {
  $(row).find('td[data-comment=true]').each(function() {
    var div = $('<div>').html('&#x1F4AC;').addClass('comment-available');
    $(this).append(div);
  });
}

function processCellData(row, data, dataIndex) {
  addDataAttributes(row, data, dataIndex);
  addBatteryBarBackground(row, data, dataIndex);
  addSpeechBubble(row, data, dataIndex);
}