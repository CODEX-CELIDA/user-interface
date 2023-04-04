let docStyle = getComputedStyle(document.documentElement);

// colormap for battery bar (cell background)
let colormap = {
    '1': docStyle.getPropertyValue('--population-intervention-color').trim(),
    '0': docStyle.getPropertyValue('--population-color').trim(),
    '-1': docStyle.getPropertyValue('--none-color').trim()
};

function findTrueElementIndex(arr, i) {
    // given a boolean area, this function returns the index of the i-th "true" element
    let count = 0;
    for (let index = 0; index < arr.length; index++) {
        if (arr[index] === true) {
            count++;
            if (count - 1 === i) {
                return index;
            }
        }
    }
    return -1; // Return -1 if the i-th true element is not found
}

function getColumnNames(api) {
    // get columns names of the data table
    // see https://datatables.net/forums/discussion/48899/cant-access-columns-names
    let colNames = api.settings()[0].aoColumns.map(function (col) {
        return col.sName;
    })

    return (colNames);
}

function addDataAttributes(api, row, data, dataIndex) {
    // add data-* attributes to each table cell (called from rowCallback)
    // specifically, each cell in each .data column is appended with a data-percantage,
    // data-days and data-comment attribute
    let visibleCols = api.columns().visible();
    let colNames = getColumnNames(api);

    $(row).find('td').each(function (colIndex) {
        
        // colIndex is the index of the td, i.e. only of the visible columns
        // need to determine the actual column index in the data
        let colIndexData = findTrueElementIndex(visibleCols, colIndex);
        let colName = colNames[colIndexData];
        
        // only add data-* attributes to columns ending with ".data"
        if (!colName.endsWith('.data')) {
            return;
        }
        
        // cut the ".data"
        let prefix = colName.slice(0, -5);

        let colIndexDays = api.column(prefix + '.days:name').index();
        let colIndexComment = api.column(prefix + '.comment:name').index();

        let dataPercentage = data[colIndexData];
        let dataDays = data[colIndexDays];
        let dataComment = data[colIndexComment];

        $(this).text((parseFloat(dataPercentage)).toFixed(0) + '%');
        $(this).attr('data-percentage', dataPercentage);
        $(this).attr('data-days', dataDays);
        $(this).attr('data-comment', dataComment);
        $(this).addClass("data-cell");
    });
}

function addBatteryBarBackground(row, data, dataIndex) {
    // for each table cell that has the data-days attribute defined,
    // this function adds a "battery-bar" background based on the data-days string
    $(row).find('td[data-days]').each(function () {
        var days = $(this).attr('data-days')
        var days_arr = days.split('').map(str => parseInt(str) - 1);

        $(this).css('border', 'solid black 1px');
        $(this).css('background', styleBatteryBar(days_arr, colormap));
        $(this).css('background-repeat', 'no-repeat');
        $(this).css('background-position', 'center');
        $(this).css('background-size', '98% 88%')
    });
}

function addSpeechBubble(row, data, dataIndex) {
    // for each table cell where the data-comment attribute is "true",
    // a div containing a speech bubble is added
    $(row).find('td[data-comment=true]').each(function () {
        var div = $('<div>').html('&#x1F4AC;').addClass('comment-available');
        $(this).append(div);
    });
}

function processCellData(api, row, data, dataIndex) {
    // called by rowCallback
    addDataAttributes(api, row, data, dataIndex);
    addBatteryBarBackground(row, data, dataIndex);
    addSpeechBubble(row, data, dataIndex);
}



function footerSummary(api, row, data, start, end, display) {
    // calculate the footer summary - called by footerCallback
    let floatVal = function (i, match) {
        // helper function to convert string to float
        if (Array.isArray(i)) {
            return floatVal(i[0]);
        }
        return parseFloat(i);
    };

    var columnVisibility = api.columns().visible();

    for (i = 0; i < api.columns().nodes().length; i++) {
        // loop only through visible columns
        if (!columnVisibility[i]) {
            continue;
        }

        // loop only through columns where the first (and therefor all) cells have a data-days attribute,
        // indicating that this is a data column and not some kind of metadata columns such as Ward, Comment etc
        let c = api.column(i, { search: 'applied' });
        let firstCell = $(api.cell(0, i).node())

        if (firstCell.data("days") === undefined) {
            continue;
        }
        
        // calculate the average of visible rows
        let p = c.data().reduce(function (a, b) { return floatVal(a) + floatVal(b) }, 0);
        p /= c.data().length;

        // set footer value
        $(c.footer()).html(p.toFixed(0) + '%');
    }
}

function onInitComplete(api) {
  $(document).on('hidden.bs.modal','#shiny-modal', function () {
    api.cells({selected: true}).deselect();
  });
}
