/**********************************************************************
File: cities.js
**********************************************************************/

/* Define Width & Height */
var width = 1800,
    height = 1100;

/* Define Path */
var path = d3.geo.path().projection(null);

/* Define green color scale */
var le_colors = d3.scale.threshold().range(colorbrewer.Greens[7]);

var tooltip = d3.select("body").append("div").attr("class", "tooltip");

/* Define svg */
var svg = d3.select("#svg_container").append("svg")
    .attr("class", "svg1")
    .attr("width", width/2)
    .attr("height", height/2);

function define_colors(min, max, mse) {
	
	// Make sure we're working with the right type.
	min = Number(min);
	max = Number(max);
	
	// This function sets the color scale correctly given a
	// min and a max.	
	var new_domain = [];
	
	// Find by how much to increment.
	var increment = (max - min)/5;
	
	//console.log('min');
	//console.log(min);
	//console.log('max');
	//console.log(max);
	
	for(i = 0; i < 6; i++) {
		new_domain.push(min + i*increment);
	}
	
	// If it's just the raw data, use greens.  For MSE , use reds.
	if(mse === false) {
		le_colors.range(colorbrewer.Reds[7]);
	} else {
		le_colors.range(colorbrewer.Greens[7]);
	}
	
	le_colors.domain(new_domain);
	
}


// Define the function to load the data.
function load_data(which) {
	
	// Read the checkboxes.
	
	d3.csv(which, function(error, data) {
  //console.log(data);
	if (error) console.log("Error: Data not loaded!");

	// Create an array to hold raw data values.
	var raw_helper = [];
	
	// Create an array to keep track of the min and max values.
	//var le_max = -50;

	data.forEach(function(d) {
	  
	  // Update the min and the max.
	  // Update the max based on the highest MSE.

	  // Get all of the MSE 	
	  //if(d.logerror > le_max) {
		  //le_max = d.logerror;
	  //}
	  
	  // Append to raw_helper.
	  raw_helper.push([d.bathroomcnt, d.bedroomcnt, d.calculatedfinishedsquarefeet, d.propertylandusetypeid, d.yearbuilt, d.structuretaxvaluedollarcnt, d.assessmentyear, d.id2, d.logerror, d.rf_logerror, d.stacking_logerror, d.DNN_logerror]);
	  
	  //d.bathroomcnt += d.bathroomcnt
	  //d.bedroomcnt += d.bedroomcnt
	  //d.calculatedfinishedsquarefeet += d.calculatedfinishedsquarefeet
	  //d.propertylandusetypeid += d.propertylandusetypeid
	  //d.yearbuilt += d.yearbuilt
	  //d.structuretaxvaluedollarcnt += d.structuretaxvaluedollarcnt
	  //d.assessmentyear += d.assessmentyear
	  //console.log('data test');
	  //console.log(d.logerror);
	  
	  // We only need the logerror values.  All calculations on
	  // these are done in update().
	  d.logerror = +d.logerror
	  d.rf_logerror = +d.rf_logerror
	  d.stacking_logerror = +d.stacking_logerror
	  d.DNN_logerror = +d.DNN_logerror
	  
	});
	
	//console.log('pre_define_colors');
	//console.log(le_min);
	//console.log(le_max);
	//console.log('done');
	
	// Set the colors.
	define_colors(0, 3, false);
	
	// Create the legend.	  
	  
	  /*
	  //sampleCategoricalData = ["Something", "Something Else", "Another", "This", "That", "Etc"]
	  sampleOrdinal = d3.scale.range(colorbrewer.YlGnBu[9]).domain(le_colors.domain());

	  verticalLegend = d3.svg.legend().labelFormat("none").cellPadding(5).orientation("vertical").units("MSE").cellWidth(25).cellHeight(18).inputScale(colorbrewer.Reds[7]).cellStepping(10);

	  d3.select("svg").append("g").attr("transform", "translate(600,140)").attr("class", "legend").call(verticalLegend);
	  
	  // Test.
	  */
	  console.log('hi');
	  var colorScale = d3.scale.quantize()
		.range(colorbrewer.Reds[7])
		.domain([0, 3]);
	  verticalLegend = d3.svg.legend().labelFormat("none").orientation("vertical").units("MSE").inputScale(colorScale);
	  d3.select("svg").append("g").attr("transform", "translate(600,140)").attr("class", "legend").call(verticalLegend);
	  
	  // Style it up!
	  $('.legendCells rect').css('padding', '0px');
	  $('.legendCells rect').css('stroke', 'none');
	  $('.legendCells rect').css('stroke-width', '0px');
	  
	  d3.select('.legendCells text').style('font-size', '20px');
	  $('.legendCells text').attr('font-size', '20px');
	  $('.legend text:not(.breakLabels)').attr('font-size', '28px');
	  $('.legend text:not(.breakLabels)').attr('font-weight', 'bold');
	  $('.legend text:not(.breakLabels)').attr('y', '-20');
	  
	  $('.breakLabels').attr('x', '45px');
	  $('.breakLabels').last().html($('.breakLabels').last().html() + '+');
	  
          

	//console.log(raw_helper);

	// Insert the data into the raw data div.

	// But first, collapse it with an identifiable separator.
	raw_helper = raw_helper.join('|');

	// Set the raw data div.
	document.getElementById("raw_data").innerHTML = raw_helper;

	/* Load California TopoJSON file */
	d3.json("la.json", function(error, la) {

		/* Bind data from caPop2014.csv file and ca.json file */
		la_json = topojson.feature(la, la.objects.la);
		
		// Call the binding function via the update function.
		//update();

		for (var i = 0; i < data.length; i++) {
			var geoid_val = data[i].id2;
			//var la_bathroomcnt_value = data[i].bathroomcnt;
			//var la_bedroomcnt_value = data[i].bedroomcnt;
			//var la_calculatedfinishedsquarefeet_value = data[i].calculatedfinishedsquarefeet;
			//var la_propertylandusetypeid_value = data[i].propertylandusetypeid;
			//var la_yearbuilt_value = data[i].yearbuilt;
			//var la_structuretaxvaluedollarcnt_value = data[i].structuretaxvaluedollarcnt;
			//var la_assessmentyear_value = data[i].assessmentyear;
			var la_logerror_value = data[i].logerror;
			var la_rf_logerror_value = data[i].rf_logerror;
			var la_stacking_logerror_value = data[i].stacking_logerror;
			var la_DNN_logerror_value = data[i].DNN_logerror;
			// DEBUG console.log(country_name, year_value);
			for (var j = 0; j < la_json.features.length; j++) {
			  var name = la_json.features[j].properties.name;
			  if (geoid_val === name) {
				//la_json.features[j].properties.bathroom = la_bathroomcnt_value;
				//la_json.features[j].properties.bedroom = la_bedroomcnt_value;
				//la_json.features[j].properties.sqft = la_calculatedfinishedsquarefeet_value;
				//la_json.features[j].properties.landusetype = la_propertylandusetypeid_value;
				//la_json.features[j].properties.constructionyear = la_yearbuilt_value;
				//la_json.features[j].properties.taxvalue = la_structuretaxvaluedollarcnt_value;
				//la_json.features[j].properties.assessyear = la_assessmentyear_value;
				la_json.features[j].properties.levalue = la_logerror_value;
				la_json.features[j].properties.rflevalue = la_rf_logerror_value;
				la_json.features[j].properties.stacklevalue = la_stacking_logerror_value;
				la_json.features[j].properties.dnnlevalue = la_DNN_logerror_value;
				break;
			  }
			}
		  }

		// This is the initial display.

		/* Display data(county name, population) and chloropleth */
		svg.selectAll("path")
		  .data(la_json.features)
		  .enter()
		  .append("path")
		  .attr("d", path)
		   //.style("fill", function(d) {
			 //if (d.properties.levalue) return le_colors(d.properties.levalue);
			 //else return "grey"});
			 
			 update()


			 var la_mesh = topojson.mesh(la, la.objects.la);
				 svg.append("path")
				   .datum(la_mesh, function(a, b) { return a !== b; })
				   .attr("class", "boundary")
				   .attr("d", path);

	});  /* end d3.json function */
});  /* end d3.csv function */
	
}


function update() {
  
  // Set up an array to store the overall search.
  var master_filter = [];
  
  // Get all the values from the text areas.
  $('textarea').each(function() {
	  
	  // The field-by-field values to filter by are saved in an array.
	  var filter_values = [];
	  
	  // Split the values.
	  var lines = $(this).val().split('\n');
	  
	  for(var i = 0; i < lines.length; i++) {
		
		// lines[i] which will give you each line
		
		// Each line is parsed to see if it contains valid data.
		// Valid data means we have only numbers, dashes, and commas.
		
		var current_line = lines[i];	
		
		// First, split on commas and trim.
		var comma_split = current_line.split(',');
		
		//console.log('comma_split');
		//console.log(comma_split);
		//console.log(comma_split[0]);
		//console.log(comma_split.length);
		//console.log('done');
		
		// If we have anything, split on the dashes.
		for(var j = 0; j < comma_split.length; j++) {
			
			//console.log('TRYING');
			//console.log(comma_split[j]);
			//console.log('GO');
			
			// Trim it first.
			var trimmable = comma_split[j].trim();
			
			var dashes_split = trimmable.split('-');
			
			//console.log('dashes_split');
			//console.log(dashes_split);
			//console.log('stop');
			
			// If there is nothing further to analyze, see if we have
			// a number.
			
			if(dashes_split.length == 1) {
				
				// Just check to see if it's a number.
				if(!(isNaN(Number(trimmable[0])))) {
					
					//console.log('pushing');
					//console.log('trimmable');
					//console.log(trimmable);
					//console.log('finally');
					// Append it to our filter values.
					filter_values.push(Number(trimmable));
				}
				
			} else {
				
				// Would have more advanced input error checking here...
				
				//console.log('dash');
				//console.log(dashes_split);
				// See how long the sequence is.
				var sequence_start = Number(dashes_split[0]);
				var sequence_end = Number(dashes_split[1]);
				
				//console.log('type check');
				//console.log(typeof(sequence_start))
				
				// Split what we have and create filter_values.
				for(var k = sequence_start; k <= sequence_end; k++) {
					//console.log('PUSHY');
					filter_values.push(k);
					
				}
				
			}
			
		}
		
		// If the field was blank, store a place holder to show so.
		if(filter_values.length == 0) {
			filter_values.push('+');
		}
		
	}
	
	// Now store the value in the master search, making sure to keep
	// only unique values.
	var search_values = Array.from(new Set(filter_values))
	master_filter.push(search_values);
		
		//console.log('FILTER_VALUES');
		//console.log(filter_values);
		//console.log('END OF LOOP');
		
  });
  
  // Get which algorithms we're using.
  var algorithms = []
  
  $('#algorithm_options input').each(function(index, value) {
	  
	  if($(this).is(':checked')) {
		  algorithms.push(index);
	  }
	  
  });
  
  var average = [];
  
  // Get if we're using any averaging.
  if(algorithms.length > 1) {
	  
		$('#average_options input').each(function(index, value) {
			
				if($(this).is(':checked')) {
			  average.push(index);
		  }
			
		});
	  
  }
  
  //console.log(algorithms);
  //console.log(average);
  
  // What is the overall search, removing duplicate values?
  
  console.log('master_filter');
  console.log(master_filter);
  console.log('stop');
  
  // Now to actually filtering records.  We just need to find
  // the ids which match our conditions.  If the filters are
  // such that we have to treat multiple logerror results,
  // we'll do that too.
  
  // First define the array to store location ids.
  var master_array = {'placeholder': 'nothing here'};
  
  // Now get the array raw data.
  var from_div = document.getElementById("raw_data").innerHTML.split('|');
  
  //console.log('from_div');
  //console.log(from_div);
  //console.log('done');
  
  // Go over each record.  
  for(var i = 0; i < from_div.length; i++) {
	  
	  // The actual record.
	  var record = from_div[i].split(',');
	  
	  //console.log('START RECORD');
	  //console.log(record);
	  //console.log('END RECORD');
	  
	  // Create a flag to see if this record should not 
	  // be included.
	  var include = true;
	  
	  //console.log('NEW RECORD');
	  
	  // Go over each filter and algorithm selection.
	  for(var j = 0; j < master_filter.length; j++) {
		  
			// Does this record meet the condition?
	  
			// Only evaluate this field if the search value is not a '+'.
			//console.log('master_filter[j]');
			//console.log(master_filter[j]);
			//console.log('all done');
			//console.log(master_filter[j] == '+');
			if(master_filter[j] != '+') {
				//console.log('field is not blank, checking record');
				//console.log(record)
				//console.log(record[j]);
				//console.log(record[j].indexOf(master_filter[j]));
				//console.log('finish');
				
				// Make sure the types match.
				var record_helper = String(record[j]);
				var master_filter_helper = String(master_filter[j]);
				
				//console.log('comparison start');
				//console.log(record_helper);
				//console.log(master_filter_helper);
				//console.log('comparison end');
				
				if(master_filter_helper.indexOf(record_helper) > -1) {
				
					// The record matches for this field.
					//console.log('match');
				
				} else {
					
					// The record doesn't match for this field.
					include = false;
					break;
					
				}
				
				
				//console.log(x);
			}		
		  
	  }
	  
	  //console.log('include value');
	  //console.log(include);
	  //console.log('done');
	  
	  // Only keep it if the flag is right.
	  if(include === true) {
		  
		  // First, note that the id column (11) is the key
		  // for the master array.		  
		  var key = String(record[7]);
		  
		  //console.log('key!')
		  //console.log(key);
		  //console.log('done');
		  
		  // Now check the algorithm selections.
		  if(algorithms.length == 1) {
			  //console.log('here');
			  //console.log('record');
			  //console.log(record);
			  //console.log('algorithms[0]');
			  //console.log(algorithms[0]);
			  //console.log('done');
			  
			  // Define the value we're adding.
			  var zillow = record[8];
			  var le_estimate = record[8 + algorithms[0]];
			  
			  // Only one algorithm is selected, so just take that data
			  // and store it in master array.
			  
			  // Note that we always have to store the Zillow le so that
			  // we can calculate MSE below.
			  if(!(key in master_array)) {
				  
				  // Initialize it.
				  master_array[key] = zillow + '+' + le_estimate;
				  
			  } else {
				  
				  master_array[key] = master_array[key] + '|' + zillow + '+' + le_estimate;
				  
			  }
			  
		  } else {
			  
			  // We have more than one algorithm in play, so we need
			  // to use the averaging option to get the MSE later.
			  
			  // First define our values.
			  var zillow = record[8];			  
			  var le_estimate_helper = [];
			  
			  for(var n = 0; n < algorithms.length; n++) {
				  //console.log('pushing')
				  //console.log(n);
				  //console.log(7 + algorithms[n]);
				  // Add each algorithm's value.
				  le_estimate_helper.push(Number(record[8 + algorithms[n]]));
				  
			  }
			  
			  // just for testing.
			  //console.log('le_estimate_helper');
			  //console.log(le_estimate_helper);
			  //console.log('done');
			  
			  
			  // Now see if we have mean or median treatment.
			  if($('#average_options input:eq(0)').is(':checked')) {
				  
				  // We take the mean.
				  var total = 0, p;
					for (p = 0; p < le_estimate_helper.length; p += 1) {
						total += le_estimate_helper[p];
					}
					le_estimate_helper = String(total / le_estimate_helper.length);
								  
			  } else {
				  
				  // We take the median.				  
				  var median = 0, numsLen = le_estimate_helper.length;
					le_estimate_helper.sort();
				 
					if (
						numsLen % 2 === 0 // is even
					) {
						// average of two middle numbers
						median = (le_estimate_helper[numsLen / 2 - 1] + le_estimate_helper[numsLen / 2]) / 2;
					} else { // is odd
						// middle number only
						median = le_estimate_helper[(numsLen - 1) / 2];
					}
				 
					le_estimate_helper = String(median);
				  
			  }
			  
			  // Note that we always have to store the Zillow le so that
			  // we can calculate MSE below.
			  if(!(key in master_array)) {
				  
				  // Initialize it.
				  master_array[key] = zillow + '+' + le_estimate_helper;
				  
			  } else {
				  
				  master_array[key] = master_array[key] + '|' + zillow + '+' + le_estimate_helper;
				  
			  }
			  
		  }
		  
	  }
	  
  }
  
  // Check the master_array.
  //console.log('master_array');
  //console.log(master_array);
  //console.log(master_array.length);
  //console.log('done');
  
  // Create a key helper to for the actual graphing.
  var key_helper = [];
  
  // Now calculate MSE for each census track and block.
  for (var key in master_array) {
		if (master_array.hasOwnProperty(key)) {
			if(key != 'placeholder') {
				
				// Calculate the MSE for this key (census tract and block).
				
				// Define a variable to hold the prediction differences.
				var sum_differences = 0;
				
				// Split the string up.
				var split_up = master_array[key].split('|');
				
				//console.log('split_up check');
				//console.log(split_up);
				//console.log('done');
				
				// What is n?
				var n_predictions = split_up.length;
				
				for(var split_index = 0; split_index < n_predictions; split_index++) {
					
					// Now split again to get the prediction and actual value.
					var split_again = split_up[split_index].split('+');
					
					// Get the difference, square it, and add it to differences.
					//console.log('calc check');
					//console.log(Number(split_again[1]));
					//console.log(Number(split_again[0]));
					//console.log(Math.pow(Number(split_again[0]) - Number(split_again[1]), 2));
					//console.log('calc check done');
					sum_differences += Math.pow(Number(split_again[0]) - Number(split_again[1]), 2);
					
				}
				
				// Collapse the differences
				
				// Now get the MSE.
				var MSE = (1/n_predictions)*sum_differences;
				
				// Set the key value.
				master_array[key] = MSE;
				
				// Save to our helper.
				key_helper.push(key);
				
				//console.log(key + " -> " + master_array[key]);
			}        
		}
	}
	
	// What is the MSE?
	//console.log('MSE');
	//console.log(master_array);
	//console.log(key_helper);
	//console.log('done');
  
  //console.log(la_json.features);
  svg.selectAll("path")
       .data(la_json.features)
       .transition().duration(1000)

  .style("fill",
  function(d) {
	  //console.log(typeof(d.properties.name));
	  //var myarr = ["I", "like", "turtles"];
	  //var arraycontainsturtles = (myarr.indexOf("turtles") > -1);
	  
		//if (d.properties.value && d.properties.name) {
		if (key_helper.indexOf(d.properties.name) > -1) {
			
			return le_colors(master_array[d.properties.name]);
			
		} else {
			//console.log('missing');
			//console.log(d)
			return "grey";
			
		}
	});
	
}