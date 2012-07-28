function drawPoint(prim) {
  var mesh, pointgeom, pointmat, tmpvertex;

  // console.log("drawPoint");

  pointgeom = new THREE.Geometry();
  for (var i = 0; i < prim.coords.length; i++) {
    tmpvertex = new THREE.Vector3(prim.coords[i][0][0], prim.coords[i][0][1], prim.coords[i][0][2]);
    pointgeom.vertices.push(tmpvertex);
  }

  var color = new THREE.Color().setRGB(prim.color[0], prim.color[1], prim.color[2]);

  pointmat = new THREE.ParticleBasicMaterial({color: color.getHex(), size: 0.05 });

  mesh = new THREE.ParticleSystem(pointgeom, pointmat);

  return(mesh);
}

function drawLine(prim) {
  var mesh, linegeom, linemat, tmpvertex;

  // console.log("drawLine");

  linegeom = new THREE.Geometry();

  for (var i = 0; i < prim.coords.length; i++) {
    tmpvertex = new THREE.Vector3(prim.coords[i][0][0], prim.coords[i][0][1], prim.coords[i][0][2]);
    linegeom.vertices.push(tmpvertex);
  }

  var color = new THREE.Color().setRGB(prim.color[0], prim.color[1], prim.color[2]);

  linemat = new THREE.LineBasicMaterial({color: color.getHex()});

  mesh = new THREE.Line(linegeom, linemat);

  // These three lines prevent grid from being put on the wrong side
  mesh.material.polygonOffset = true;
  mesh.material.polygonOffsetFactor = 1;
  mesh.material.polygonOffsetUnits = 1;

  return(mesh);
}

function drawPolygon(prim) {    
  var mesh, polypath, polyshape, polygeom, material;

  // console.log("drawPolygon");

  // Find three points
  var p1 = new THREE.Vector4(prim.coords[0][0][0], prim.coords[0][0][1], prim.coords[0][0][2]);
  var p2 = new THREE.Vector4(prim.coords[1][0][0], prim.coords[1][0][1], prim.coords[1][0][2]);
  var p3 = new THREE.Vector4(prim.coords[2][0][0], prim.coords[2][0][1], prim.coords[2][0][2]);

  if (prim.coords.length == 3) {    // Fast Return
    polygeom = new THREE.Geometry();
    polygeom.vertices.push(p1);
    polygeom.vertices.push(p2);
    polygeom.vertices.push(p3);
    polygeom.faces.push(new THREE.Face3(0, 1, 2));
    polygeom.faces.push(new THREE.Face3(0, 2, 1));
  } else {
     //TODO: Check the 3 points are not colinear

    // And Three Vectors
    var v1 = new THREE.Vector3().sub(p2, p1);
    var v2 = new THREE.Vector3().sub(p3, p1);
    var v3 = new THREE.Vector3().cross(v1, v2); // normal vector

    var normal = new THREE.Vector4(v3.x, v3.y, v3.z, -v3.dot(p1));  // Point p on the plane iff p.normal = 0

    // Point Closest to origin - translation
    var nearest = new THREE.Vector3(
              normal.x*normal.w / (normal.x*normal.x + normal.y*normal.y + normal.z*normal.z),
              normal.y*normal.w / (normal.x*normal.x + normal.y*normal.y + normal.z*normal.z),
              normal.z*normal.w / (normal.x*normal.x + normal.y*normal.y + normal.z*normal.z)
    );

    // Angles to the z axis - rotaton
    var thetax = Math.acos(normal.z / Math.sqrt(normal.y*normal.y + normal.z*normal.z));
    var thetay = Math.acos(normal.z / Math.sqrt(normal.x*normal.x + normal.z*normal.z));
    
    // Linear Transformation Matrix - Rotation + Translation
    var L = new THREE.Matrix4();
    L.makeTranslation(-nearest.x, -nearest.y, -nearest.z);  
    L.multiplySelf(new THREE.Matrix4().makeRotationX(thetax));
    L.multiplySelf(new THREE.Matrix4().makeRotationY(thetay));

    polypath = new THREE.Path();
    for (var i = 0; i < prim.coords.length; i++) {
      tmpv = new THREE.Vector4(prim.coords[i][0][0], prim.coords[i][0][1], prim.coords[i][0][2], 1);
      L.multiplyVector4(tmpv);
      if (i == 0){
          polypath.moveTo(tmpv.x, tmpv.y);
      } else {
          polypath.lineTo(tmpv.x, tmpv.y);
      }
    }
    polypath.lineTo(polypath.curves[0].v1.x, polypath.curves[0].v1.y); // Close the curve

    polyshape = polypath.toShapes();
    polygeom = new THREE.ExtrudeGeometry(polyshape, {amount:0.0, steps:0, bevelEnabled: false});

    // Undo the Linear Transformation
    for (var i=0; i < polygeom.vertices.length; i++) {
        polygeom.vertices[i].x = prim.coords[i][0][0];
        polygeom.vertices[i].y = prim.coords[i][0][1];
        polygeom.vertices[i].z = prim.coords[i][0][2];
    }
  }

  polygeom.computeFaceNormals();

  var color = new THREE.Color().setRGB(prim.faceColor[0], prim.faceColor[1], prim.faceColor[2]);
  //polymat = new THREE.MeshLambertMaterial({color: color.getHex(), transparent: true, opacity: prim.faceColor[3]});
  polymat = new THREE.MeshPhongMaterial({color: color.getHex(), transparent: true, opacity: prim.faceColor[3]});

  mesh = new THREE.Mesh(polygeom, polymat);
  return mesh;
}

function drawGraphics3D(container, data) {
  // data is decoded JSON data such as
  // {"elements": [{"coords": [[[1.0, 0.0, 0.0], null], [[1.0, 1.0, 1.0], null], [[0.0, 0.0, 1.0], null]], "type": "polygon", "faceColor": [0, 0, 0, 1]}], "axes": {}, "extent": {"zmax": 1.0, "ymax": 1.0, "zmin": 0.0, "xmax": 1.0, "xmin": 0.0, "ymin": 0.0}, "lighting": []}
  // The nulls are the "scaled" parts of coordinates that depend on the 
  // size of the final graphics (see Mathematica's Scaled). TODO.

  // BoxRatios option is stored in data.boxratios and is either Automatic or a length 3 array.

  // TODO: update the size of the container dynamically
  // (we also need some mechanism to update the enclosing <mspace>).

  // Axes are created using the information in data.axes such as
  // {"axes": {"hasaxes": [true, true, false], "ticks": [[-1, 0, 1], [-2, 0, 2], [0., 0.5, 1]]}}.

  // Lights are created using the information in data.lighing such as
  // {"type": "Ambient", "color": [0.3, 0.2, 0.4]}
  // {"type": "Directional", "color": [0.3, 0.2, 0.4], "position": [2, 0, 2]}

  // TODO: Shading, handling of VertexNormals.

  var camera, scene, renderer, boundbox,
    isMouseDown = false, onMouseDownPosition, radius,
    tmpx, tmpy, tmpz, 
    theta = Math.PI/4, onMouseDownTheta = Math.PI/4, phi = Math.PI/3, onMouseDownPhi = Math.PI/3;

  // BoxRatios induce scaling
  if (data.boxratios == 'Automatic') {
    boxscale = new THREE.Vector3(1,1,1);
  } else {
    if ((data.boxratios instanceof Array) && (data.boxratios.length == 3)) {
      boxscale = new THREE.Vector3(
        data.boxratios[0] / (data.extent["xmax"] - data.extent["xmin"]),
        data.boxratios[1] / (data.extent["ymax"] - data.extent["ymin"]),
        data.boxratios[2] / (data.extent["zmax"] - data.extent["zmin"])
      );
    } else {
      alert("Error: Internal Boxratios not Automatic or Array(3)");
      return;
    }
  }

  // Center of the scene
  var center = new THREE.Vector3(
    0.5 * boxscale.x * (data.extent["xmin"] + data.extent["xmax"]),
    0.5 * boxscale.y * (data.extent["ymin"] + data.extent["ymax"]), 
    0.5 * boxscale.z * (data.extent["zmin"] + data.extent["zmax"]));

  // Where the camera is looking
  var focus = new THREE.Vector3(center.x, center.y, center.z);

  radius = 2 * Math.sqrt(
   Math.pow(boxscale.x * (data.extent["xmax"] - data.extent["xmin"]), 2) +
   Math.pow(boxscale.y * (data.extent["ymax"] - data.extent["ymin"]), 2) +
   Math.pow(boxscale.z * (data.extent["zmax"] - data.extent["zmin"]), 2));

  // Scene
  scene = new THREE.Scene();
  scene.position = center;

  camera = new THREE.PerspectiveCamera(
    35,             // Field of view
    1,            // Aspect ratio
    0.1*radius,     // Near plane
    1000*radius     // Far plane
  );

  function update_camera_position() {
    camera.position.x = focus.x + radius * Math.sin(theta) * Math.cos(phi);
    camera.position.y = focus.y + radius * Math.cos(theta) * Math.cos(phi);
    camera.position.z = focus.z + radius * Math.sin(phi);
    camera.lookAt(focus);
  }

  update_camera_position();
  camera.up = new THREE.Vector3(0,0,1);

  scene.add(camera);

  // Lighting
  function addLight(l) {
    var color = new THREE.Color().setRGB(l.color[0], l.color[1], l.color[2]);
    var light;

    if (l.type == "Ambient") {
      light = new THREE.AmbientLight(color.getHex());
    } else if (l.type == "Directional") {
      light = new THREE.DirectionalLight(color.getHex(), 1);
    } else if (l.type == "Spot") {
      light = new THREE.SpotLight(color.getHex());
      light.position.set(boxscale.x * l.position[0], boxscale.y * l.position[1], boxscale.z * l.position[2]);
      light.target.position.set(boxscale.x * l.target[0], boxscale.y * l.target[1], boxscale.z * l.target[2]);
      light.target.updateMatrixWorld(); // This fixes bug in THREE.js
      light.angle = l.angle;
    } else if (l.type == "Point") {
      light = new THREE.PointLight(color.getHex())
      light.position.set(boxscale.x * l.position[0], boxscale.y * l.position[1], boxscale.z * l.position[2]);

      // Add visible light sphere
      lightsphere = new THREE.Mesh(
        new THREE.SphereGeometry(0.007*radius, 16, 8),
        new THREE.MeshBasicMaterial({color: color.getHex()})
      );
      lightsphere.position = light.position;
      scene.add(lightsphere);
    } else {
      alert("Error: Internal Light Error", l.type);
      return;
    }
    return light;
  }

  function getInitLightPos(l) {
    // Initial Light position in spherical polar coordinates
    if (l.position instanceof Array) {
      tmppos = new THREE.Vector3(boxscale.x * l.position[0], boxscale.y * l.position[1], boxscale.z * l.position[2]);
      result = new Object();
      result.radius = radius * tmppos.length();
      if (tmppos.isZero()) {
        result.theta = 0;
        result.phi = 0;
      } else {
        result.phi = Math.acos(tmppos.z / result.radius);
        result.theta = Math.atan2(tmppos.y, tmppos.x);
      }
      return result;
    }
    return;
  }

  function positionLights() {
    for (var i = 0; i < lights.length; i++) {
      if (lights[i] instanceof THREE.DirectionalLight) {
        lights[i].position.x = focus.x + initLightPos[i].radius * Math.sin(theta + initLightPos[i].theta) * Math.cos(phi + initLightPos[i].phi);
        lights[i].position.y = focus.y + initLightPos[i].radius * Math.cos(theta + initLightPos[i].theta) * Math.cos(phi + initLightPos[i].phi);
        lights[i].position.z = focus.z + initLightPos[i].radius * Math.sin(phi + initLightPos[i].phi);
      }
    }
  }

  lights = new Array(data.lighting.length);
  initLightPos = new Array(data.lighting.length);

  for (var i = 0; i < data.lighting.length; i++) {
    initLightPos[i] = getInitLightPos(data.lighting[i]);
    
    lights[i] = addLight(data.lighting[i]);
    scene.add(lights[i]);
  }

  // BoundingBox
  boundbox = new THREE.Mesh(
    new THREE.CubeGeometry(
      boxscale.x * (data.extent["xmax"] - data.extent["xmin"]),
      boxscale.y * (data.extent["ymax"] - data.extent["ymin"]),
      boxscale.z * (data.extent["zmax"] - data.extent["zmin"])),
    new THREE.MeshBasicMaterial({color: 0x666666, wireframe: true})
  );
  boundbox.position = center;
  scene.add(boundbox);  

  // Draw the Axes
  if (data.axes.hasaxes instanceof Array) {
    hasaxes = new Array(data.axes.hasaxes[0], data.axes.hasaxes[1], data.axes.hasaxes[2]);
  } else if (data.axes.hasaxes instanceof Boolean) {
    if (data.axes) {
      hasaxes = new Array(true, true, true);
    } else {
      hasaxes = new Array(false, false, false);
    }
  } else {
    hasaxes = new Array(false, false, false);
  }
  var axesmat = new THREE.LineBasicMaterial({ color: 0x000000, linewidth : 1.5 });
  var axesgeom = new Array;
  var axesindicies = [
    [[0,5], [1,4], [2,7], [3,6]],
    [[0,2], [1,3], [4,6], [5,7]],
    [[0,1], [2,3], [4,5], [6,7]]
  ];

  axesmesh = new Array(3);
  for (var i=0; i<3; i++) {
    if (hasaxes[i]) {
      axesgeom[i] = new THREE.Geometry();
      axesgeom[i].vertices.push(new THREE.Vector3().add(
        boundbox.geometry.vertices[axesindicies[i][0][0]], boundbox.position)
      );
      axesgeom[i].vertices.push(new THREE.Vector3().add(
        boundbox.geometry.vertices[axesindicies[i][0][1]], boundbox.position)
      );
      axesmesh[i] = new THREE.Line(axesgeom[i], axesmat);
      axesmesh[i].geometry.dynamic = true;
      scene.add(axesmesh[i]);
    }
  }

  function boxEdgeLength(i, j) {
    edge = new THREE.Vector3().sub(
      toCanvasCoords(boundbox.geometry.vertices[axesindicies[i][j][0]]),
      toCanvasCoords(boundbox.geometry.vertices[axesindicies[i][j][1]])
    );
    edge.z = 0;
    return edge.length();
  }

  function positionAxes() {
    // Automatic axes placement
    nearj = null;
    nearl = 10*radius;
    farj = null;
    farl = 0.0;
    
    tmpv = new THREE.Vector3();
    for (var j = 0; j < 8; j++) {
      tmpv.add(boundbox.geometry.vertices[j], boundbox.position);
      tmpv.subSelf(camera.position);
      tmpl = tmpv.length();
      if (tmpl < nearl) {
        nearl = tmpl;
        nearj = j;
      } else if (tmpl > farl) {
        farl = tmpl;
        farj = j;
      }
    }
    for (var i = 0; i < 3; i++) {
      if (hasaxes[i]) {
        maxj = null;
        maxl = 0.0;
        for (var j = 0; j < 4; j++) {
          if (axesindicies[i][j][0] != nearj && axesindicies[i][j][1] != nearj && axesindicies[i][j][0] != farj && axesindicies[i][j][1] != farj) {
            tmpl = boxEdgeLength(i, j);
            if (tmpl > maxl) {
              maxl = tmpl;
              maxj = j;
            }
          }
        }
        axesmesh[i].geometry.vertices[0].add(boundbox.geometry.vertices[axesindicies[i][maxj][0]], boundbox.position);
        axesmesh[i].geometry.vertices[1].add(boundbox.geometry.vertices[axesindicies[i][maxj][1]], boundbox.position);
        axesmesh[i].geometry.verticesNeedUpdate = true;
      }
    }
    update_axes();
  }

  // Axes Ticks
  var tickmat = new THREE.LineBasicMaterial({ color: 0x000000, linewidth : 1.2 });
  var ticks = new Array(3);
  var ticks_small = new Array(3);
  var ticklength = 0.005*radius;

  for (var i = 0; i < 3; i++) {
    if (hasaxes[i]) {
      ticks[i] = new Array;
      for (var j = 0; j < data.axes.ticks[i][0].length; j++) {
        tickgeom = new THREE.Geometry();
        tickgeom.vertices.push(new THREE.Vector3());
        tickgeom.vertices.push(new THREE.Vector3());
        ticks[i].push(new THREE.Line(tickgeom, tickmat));
        scene.add(ticks[i][j]);

      }
      ticks_small[i] = new Array;
      for (var j = 0; j < data.axes.ticks[i][1].length; j++) {
         tickgeom = new THREE.Geometry();
         tickgeom.vertices.push(new THREE.Vector3());
         tickgeom.vertices.push(new THREE.Vector3());
         ticks_small[i].push(new THREE.Line(tickgeom, tickmat));
         scene.add(ticks_small[i][j]);
      }
    }
  }

  function getTickDir(i) {
    var tickdir = new THREE.Vector3();
    if (i == 0) {
      if (-45 < phi && phi < 45) {
        if (axesgeom[0].vertices[0].z > boundbox.position.z) {
          tickdir.set(0, 0, -ticklength);
        } else {
          tickdir.set(0, 0, ticklength);
        }
      } else {
        if (axesgeom[0].vertices[0].y > boundbox.position.y) {
          tickdir.set(0,-ticklength, 0);
        } else {
          tickdir.set(0, ticklength, 0);
        }
      }
    } else if (i == 1) {
      if (-45 < phi && phi < 45) {
        if (axesgeom[1].vertices[0].z > boundbox.position.z) {
          tickdir.set(0, 0, -ticklength);
        } else {
          tickdir.set(0, 0, ticklength);
        }
      } else {
        if (axesgeom[1].vertices[0].x > boundbox.position.x) {
          tickdir.set(-ticklength, 0, 0);
        } else {
          tickdir.set(ticklength, 0, 0);
        }
      }
    } else if (i == 2) {
      if ((45 < theta && theta < 135) || (225 < theta && theta < 315)) {
        if (axesgeom[2].vertices[0].y > boundbox.position.y) {
          tickdir.set(0, -ticklength, 0);
        } else {
          tickdir.set(0, ticklength, 0);
        }
      } else {
        if (axesgeom[2].vertices[0].x > boundbox.position.x) {
          tickdir.set(-ticklength, 0, 0);
        } else {
          tickdir.set(ticklength, 0, 0);
        }
      }
    }
    return tickdir;
  }

  function update_axes() {
    for (var i = 0; i < 3; i++) {
      if (hasaxes[i]) {
        tickdir = getTickDir(i);
        small_tickdir = tickdir.clone();
        small_tickdir.multiplyScalar(0.5);
        for (var j = 0; j < data.axes.ticks[i][0].length; j++) {
          tmpval = data.axes.ticks[i][0][j];

          ticks[i][j].geometry.vertices[0].copy(axesgeom[i].vertices[0]);
          ticks[i][j].geometry.vertices[1].add(axesgeom[i].vertices[0], tickdir);

          if (i == 0) {
            ticks[i][j].geometry.vertices[0].x = boxscale.x * tmpval;
            ticks[i][j].geometry.vertices[1].x = boxscale.x * tmpval;
          } else if (i == 1) {
            ticks[i][j].geometry.vertices[0].y = boxscale.y * tmpval;
            ticks[i][j].geometry.vertices[1].y = boxscale.y * tmpval;
          } else if (i == 2) {
            ticks[i][j].geometry.vertices[0].z = boxscale.z * tmpval;
            ticks[i][j].geometry.vertices[1].z = boxscale.z * tmpval;
          }

          ticks[i][j].geometry.verticesNeedUpdate = true;
        }
        for (var j = 0; j < data.axes.ticks[i][1].length; j++) {
          tmpval = data.axes.ticks[i][1][j];

          ticks_small[i][j].geometry.vertices[0].copy(axesgeom[i].vertices[0]);
          ticks_small[i][j].geometry.vertices[1].add(axesgeom[i].vertices[0], small_tickdir);

          if (i == 0) {
            ticks_small[i][j].geometry.vertices[0].x = boxscale.x * tmpval;
            ticks_small[i][j].geometry.vertices[1].x = boxscale.x * tmpval;
          } else if (i == 1) {
            ticks_small[i][j].geometry.vertices[0].y = boxscale.y * tmpval;
            ticks_small[i][j].geometry.vertices[1].y = boxscale.y * tmpval;
          } else if (i == 2) {
            ticks_small[i][j].geometry.vertices[0].z = boxscale.z * tmpval;
            ticks_small[i][j].geometry.vertices[1].z = boxscale.z * tmpval;
          }

          ticks_small[i][j].geometry.verticesNeedUpdate = true;
        }
      }
    }
  }
  update_axes();

  // Axes numbering using divs
  var ticknums = new Array(3);
  for (var i = 0; i < 3; i++) {
    if (hasaxes[i]) {
      ticknums[i] = new Array(data.axes.ticks[i][0].length);
      for (var j = 0; j < ticknums[i].length; j++) {
        ticknums[i][j] = document.createElement('div');
        ticknums[i][j].innerHTML = data.axes.ticks[i][0][j];

        // Handle Minus signs
        if (data.axes.ticks[i][0][j] >= 0) {
          ticknums[i][j].style.paddingLeft = "0.5em";
        } else {
          ticknums[i][j].style.paddingLeft = 0;
        }

        ticknums[i][j].style.position = "absolute";
        ticknums[i][j].style.fontSize = "0.8em";
        container.appendChild(ticknums[i][j]);
      }
    }
  }
  
  function toCanvasCoords(position) {
    var pos = position.clone();
    var projScreenMat = new THREE.Matrix4();
    projScreenMat.multiply(camera.projectionMatrix, camera.matrixWorldInverse);
    projScreenMat.multiplyVector3( pos );

    var result = new THREE.Vector3((pos.x + 1 ) * 200, (1-pos.y) * 200, (pos.z + 1 ) * 200);
    return result;
  }

  function positionticknums() {
    for (var i = 0; i < 3; i++) {
      if (hasaxes[i]) {
        for (var j = 0; j < ticknums[i].length; j++) {
          var tickpos3D = ticks[i][j].geometry.vertices[0].clone();
          var tickDir = new THREE.Vector3().sub(ticks[i][j].geometry.vertices[0], ticks[i][j].geometry.vertices[1]);
          //tickDir.multiplyScalar(3);
          tickDir.setLength(3*ticklength)
          tickDir.x *= 2.0;
          tickDir.y *= 2.0;
          tickpos3D.addSelf(tickDir);
          var tickpos = toCanvasCoords(tickpos3D);
          tickpos.x -= 10;
          tickpos.y += 8;

          ticknums[i][j].style.left = tickpos.x.toString() + "px";
          ticknums[i][j].style.top = tickpos.y.toString() + "px";
          if (tickpos.x < 5 || tickpos.x > 395 || tickpos.y < 5 || tickpos.y > 395) {
            ticknums[i][j].style.display = "none";
          }
          else {
            ticknums[i][j].style.display = "";
          }
        }
      }
    }
  }

  function applyBoxScaling(mesh) {
    for (var i = 0; i < mesh.geometry.vertices.length; i++) {
      mesh.geometry.vertices[i].x -= center.x / boxscale.x;
      mesh.geometry.vertices[i].y -= center.y / boxscale.y;
      mesh.geometry.vertices[i].z -= center.z / boxscale.z;

      mesh.geometry.vertices[i].x *= boxscale.x;
      mesh.geometry.vertices[i].y *= boxscale.y;
      mesh.geometry.vertices[i].z *= boxscale.z;

      mesh.geometry.vertices[i].addSelf(center);
    }
    return mesh;
  }

  // Plot the primatives
  for (var indx = 0; indx < data.elements.length; indx++) {
    var type = data.elements[indx].type;
    switch(type) {
      case "point":
        scene.add(applyBoxScaling(drawPoint(data.elements[indx])));
        break
      case "line":
        scene.add(applyBoxScaling(drawLine(data.elements[indx])));
        break;
      case "polygon":
        scene.add(applyBoxScaling(drawPolygon(data.elements[indx])));
        break;
      default:
        alert("Error: Unknown type passed to drawGraphics3D");
    }
  }

  // Renderer
  renderer = new THREE.WebGLRenderer({antialias: true});
  renderer.setSize(400, 400);
  container.appendChild(renderer.domElement);

  function render() {
    positionLights();
    renderer.render( scene, camera );
  };

  function toScreenCoords(position) {
    var camz = new THREE.Vector3().sub(focus, camera.position);
    camz.normalize();

    var camx = new THREE.Vector3(
        radius * Math.cos(phi) * Math.cos(theta),
        - radius * Math.cos(phi) * Math.sin(theta),
        0
    );
    camx.normalize();

    var camy = new THREE.Vector3();
    camy.cross(camz, camx);

    var campos = new THREE.Vector3().sub(position, camera.position);
    campos.addSelf(focus);

    var cam = new THREE.Vector3(
        camx.dot(campos),
        camy.dot(campos),
        camz.dot(campos)
    );
    
    return cam;
  }

  function ScaleInView() {
    var tmp_fov = 0.0;

    for (var i=0; i<8; i++) {
      proj2d = toScreenCoords(boundbox.geometry.vertices[i]);

      angle = 57.296 * Math.max(
         Math.abs(Math.atan(proj2d.x/proj2d.z) / camera.aspect),
         Math.abs(Math.atan(proj2d.y/proj2d.z))
      );
      tmp_fov = Math.max(tmp_fov, 2*angle);
    }

    camera.fov = tmp_fov + 5;
    camera.updateProjectionMatrix();
  }

  // Mouse Interactions
  function onDocumentMouseDown( event ) {
    event.preventDefault();

    isMouseDown = true;
    isShiftDown = false;
    isCtrlDown = false;

    onMouseDownTheta = theta;
    onMouseDownPhi = phi;

    onMouseDownPosition.x = event.clientX;
    onMouseDownPosition.y = event.clientY;

    onMouseDownFocus = new THREE.Vector3().copy(focus);
  }

  function onDocumentMouseMove(event) {
    event.preventDefault();

    if (isMouseDown) {
      positionticknums();

      if (event.shiftKey) {
        // console.log("Pan");
        if (! isShiftDown) {
          isShiftDown = true;
          onMouseDownPosition.x = event.clientX;
          onMouseDownPosition.y = event.clientY;
          autoRescale = false;
          container.style.cursor = "move";
        }
        var camz = new THREE.Vector3().sub(focus, camera.position);
        camz.normalize();

        var camx = new THREE.Vector3(
            radius * Math.cos(phi) * Math.cos(theta),
            - radius * Math.cos(phi) * Math.sin(theta),
            0
        );
        camx.normalize();

        var camy = new THREE.Vector3();
        camy.cross(camz, camx);

        focus.x = onMouseDownFocus.x + (radius / 400)*(camx.x * (event.clientX - onMouseDownPosition.x) + camy.x * (event.clientY - onMouseDownPosition.y));
        focus.y = onMouseDownFocus.y + (radius / 400)*(camx.y * (event.clientX - onMouseDownPosition.x) + camy.y * (event.clientY - onMouseDownPosition.y));
        focus.z = onMouseDownFocus.z + (radius / 400)*(camx.z * (event.clientX - onMouseDownPosition.x) + camy.z * (event.clientY - onMouseDownPosition.y));

        update_camera_position();

      } else if (event.ctrlKey) {
        // console.log("Zoom");
        if (! isCtrlDown) {
          isCtrlDown = true;
          onCtrlDownFov = camera.fov;
          onMouseDownPosition.x = event.clientX;
          onMouseDownPosition.y = event.clientY;
          autoRescale = false;
          container.style.cursor = "crosshair";
        }
        camera.fov =  onCtrlDownFov + 20 * Math.atan((event.clientY - onMouseDownPosition.y)/50);
        camera.fov = Math.max(1, Math.min(camera.fov, 150));
        camera.updateProjectionMatrix();

      } else {
        // console.log("Spin");
        if (isCtrlDown || isShiftDown) {
          onMouseDownPosition.x = event.clientX;
          onMouseDownPosition.y = event.clientY;
          isShiftDown = false;
          isCtrlDown = false;
          container.style.cursor = "pointer";
        }

        theta = 2 * Math.PI * (event.clientX - onMouseDownPosition.x) / 400 + onMouseDownTheta;
        theta = (theta + 2 * Math.PI) % (2 * Math.PI);
        phi = 2 * Math.PI * (event.clientY - onMouseDownPosition.y) / 400 + onMouseDownPhi;
        phi = Math.max(Math.min(0.5 * Math.PI, phi), -0.5 * Math.PI);

        update_camera_position();
      }
      render();

    } else {
        container.style.cursor = "pointer";
    }
  }

  function onDocumentMouseUp(event) {
    event.preventDefault();

    isMouseDown = false;
    container.style.cursor = "pointer";

    if (autoRescale) {
        ScaleInView();
        render();
    }
    positionAxes();
    render();
    positionticknums();
  }

  // Bind Mouse events
  container.addEventListener('mousemove', onDocumentMouseMove, false);
  container.addEventListener('mousedown', onDocumentMouseDown, false);
  container.addEventListener('mouseup', onDocumentMouseUp, false);
  onMouseDownPosition = new THREE.Vector2();
  autoRescale = true;

  update_camera_position();
  ScaleInView();
  positionAxes();
  render();
  positionticknums();
}

