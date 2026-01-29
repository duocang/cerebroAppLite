// layout for 2D projections
var overview_projection_uirevision = 'true';

const overview_projection_layout_2D = {
  // uirevision will be set dynamically
  hovermode: 'closest',
  dragmode: 'select', // Enable selection mode for cell selection
  margin: {
    l: 50,
    r: 50,
    b: 50,
    t: 50,
    pad: 4,
  },
  legend: {
    itemsizing: 'constant',
  },
  xaxis: {
    autorange: true,
    mirror: true,
    showline: true,
    zeroline: false,
    range: [],
  },
  yaxis: {
    autorange: true,
    mirror: true,
    showline: true,
    zeroline: false,
    range: [],
  },
  hoverlabel: {
    font: {
      size: 11,
    },
    align: 'left',
  },
};

// layout for 3D projections
const overview_projection_layout_3D = {
  // uirevision will be set dynamically
  hovermode: 'closest',
  margin: {
    l: 50,
    r: 50,
    b: 50,
    t: 50,
    pad: 4,
  },
  legend: {
    itemsizing: 'constant',
  },
  scene: {
    xaxis: {
      autorange: true,
      mirror: true,
      showline: true,
      zeroline: false,
      range: [],
    },
    yaxis: {
      autorange: true,
      mirror: true,
      showline: true,
      zeroline: false,
      range: [],
    },
    zaxis: {
      autorange: true,
      mirror: true,
      showline: true,
      zeroline: false,
    },
  },
  hoverlabel: {
    font: {
      size: 11,
    },
    align: 'left',
  },
};

// structure of input data
const overview_projection_default_params = {
  meta: {
    color_type: '',
    traces: [],
    color_variable: '',
  },
  data: {
    x: [],
    y: [],
    z: [],
    color: [],
    size: '',
    opacity: '',
    line: {},
    x_range: [],
    y_range: [],
    reset_axes: false,
  },
  hover: {
    hoverinfo: '',
    text: [],
  },
  group_centers: {
    group: [],
    x: [],
    y: [],
    z: [],
  },
};

// update 2D projection with continuous coloring
shinyjs.updatePlot2DContinuous = function (params) {
  params = shinyjs.getParams(params, overview_projection_default_params);
  const data = [];
  data.push({
    x: params.data.x,
    y: params.data.y,
    mode: 'markers',
    type: 'scattergl',
    marker: {
      size: params.data.point_size,
      opacity: params.data.point_opacity,
      line: params.data.point_line,
      color: params.data.color,
      colorscale: 'YlGnBu',
      reversescale: true,
      colorbar: {
        title: {
          text: params.meta.color_variable,
        },
      },
    },
    hoverinfo: params.hover.hoverinfo,
    text: params.hover.text,
    showlegend: false,
  });
  const layout_here = JSON.parse(JSON.stringify(overview_projection_layout_2D));

  if (params.data.reset_axes) {
    overview_projection_uirevision = Date.now().toString();
    layout_here.xaxis['autorange'] = true;
    layout_here.yaxis['autorange'] = true;
  } else {
    layout_here.xaxis['autorange'] = false;
    layout_here.xaxis['range'] = params.data.x_range;
    layout_here.yaxis['autorange'] = false;
    layout_here.yaxis['range'] = params.data.y_range;
  }
  layout_here.uirevision = overview_projection_uirevision;

  Plotly.react('overview_projection', data, layout_here);
};

// update 3D projection with continuous coloring
shinyjs.updatePlot3DContinuous = function (params) {
  params = shinyjs.getParams(params, overview_projection_default_params);
  const data = [];
  data.push({
    x: params.data.x,
    y: params.data.y,
    z: params.data.z,
    mode: 'markers',
    type: 'scatter3d',
    marker: {
      size: params.data.point_size,
      opacity: params.data.point_opacity,
      line: params.data.point_line,
      color: params.data.color,
      colorscale: 'YlGnBu',
      reversescale: true,
      colorbar: {
        title: {
          text: params.meta.color_variable,
        },
      },
    },
    hoverinfo: params.hover.hoverinfo,
    text: params.hover.text,
    showlegend: false,
  });

  const layout_here = JSON.parse(JSON.stringify(overview_projection_layout_3D));

  if (params.data.reset_axes) {
    overview_projection_uirevision = Date.now().toString();
  }
  layout_here.uirevision = overview_projection_uirevision;

  Plotly.react('overview_projection', data, layout_here);
};

// update 2D projection with categorical coloring
shinyjs.updatePlot2DCategorical = function (params) {
  params = shinyjs.getParams(params, overview_projection_default_params);

  // Optimization: map directly to data array
  const data = params.data.x.map((_, i) => ({
    x: params.data.x[i],
    y: params.data.y[i],
    name: params.meta.traces[i],
    mode: 'markers',
    type: 'scattergl',
    marker: {
      size: params.data.point_size,
      opacity: params.data.point_opacity,
      line: params.data.point_line,
      color: params.data.color[i],
    },
    hoverinfo: params.hover.hoverinfo,
    text: params.hover.text[i],
    hoverlabel: {
      bgcolor: params.data.color[i],
    },
    showlegend: true,
  }));

  if (params.group_centers.group.length >= 1) {
    data.push({
      x: params.group_centers.x,
      y: params.group_centers.y,
      text: params.group_centers.group,
      type: 'scattergl',
      mode: 'text',
      name: 'Labels',
      textposition: 'middle center',
      textfont: {
        color: '#000000',
        size: 16,
      },
      hoverinfo: 'skip',
      inherit: false,
    });
  }

  const layout_here = JSON.parse(JSON.stringify(overview_projection_layout_2D));

  if (params.data.reset_axes) {
    overview_projection_uirevision = Date.now().toString();
    layout_here.xaxis['autorange'] = true;
    layout_here.yaxis['autorange'] = true;
  } else {
    layout_here.xaxis['autorange'] = false;
    layout_here.xaxis['range'] = params.data.x_range;
    layout_here.yaxis['autorange'] = false;
    layout_here.yaxis['range'] = params.data.y_range;
  }
  layout_here.uirevision = overview_projection_uirevision;

  Plotly.react('overview_projection', data, layout_here);
};

// update 3D projection with categorical coloring
shinyjs.updatePlot3DCategorical = function (params) {
  params = shinyjs.getParams(params, overview_projection_default_params);

  const data = params.data.x.map((_, i) => ({
    x: params.data.x[i],
    y: params.data.y[i],
    z: params.data.z[i],
    name: params.meta.traces[i],
    mode: 'markers',
    type: 'scatter3d',
    marker: {
      size: params.data.point_size,
      opacity: params.data.point_opacity,
      line: params.data.point_line,
      color: params.data.color[i],
    },
    hoverinfo: params.hover.hoverinfo,
    text: params.hover.text[i],
    hoverlabel: {
      bgcolor: params.data.color[i],
    },
    showlegend: true,
  }));

  if (params.group_centers.group.length >= 1) {
    data.push({
      x: params.group_centers.x,
      y: params.group_centers.y,
      z: params.group_centers.z,
      text: params.group_centers.group,
      type: 'scatter3d',
      mode: 'text',
      name: 'Labels',
      textposition: 'middle center',
      textfont: {
        color: '#000000',
        size: 16,
      },
      hoverinfo: 'skip',
      inherit: false,
    });
  }

  const layout_here = JSON.parse(JSON.stringify(overview_projection_layout_3D));

  if (params.data.reset_axes) {
    overview_projection_uirevision = Date.now().toString();
  }
  layout_here.uirevision = overview_projection_uirevision;

  Plotly.react('overview_projection', data, layout_here);
};
