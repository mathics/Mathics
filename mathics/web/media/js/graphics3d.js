function drawPoint(prim) {
  var mesh, pointgeom, pointmat, tmpvertex;

  // console.log("drawPoint");

  pointgeom = new THREE.Geometry();
  for (var i = 0; i < prim.coords.length; i++) {
    tmpvertex = new THREE.Vector3(prim.coords[i][0][0], prim.coords[i][0][1], prim.coords[i][0][2]);
    pointgeom.vertices.push(tmpvertex);
  }

  pointmat = new THREE.ParticleBasicMaterial({ color: 0x000000, size: 0.05 });

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

  linemat = new THREE.LineBasicMaterial({color: 0x000000});

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
    var v1 = new THREE.Vector3();
    var v2 = new THREE.Vector3();
    var v3 = new THREE.Vector3(); // normal vector

    v1.sub(p2, p1);
    v2.sub(p3, p1);
    v3.cross(v1, v2);

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

  //mesh = new THREE.Mesh(polygeom, new THREE.MeshBasicMaterial({color: 0x000000}));
  polygeom.computeFaceNormals();
  mesh = new THREE.Mesh(polygeom, new THREE.MeshNormalMaterial());
  return mesh;
}

function drawGraphics3D(container, data) {
  // TODO: use the actual graphic given by data.
  // data is decoded JSON data such as
  // {"elements": [{"coords": [[[1.0, 0.0, 0.0], null], [[1.0, 1.0, 1.0], null], [[0.0, 0.0, 1.0], null]], "type": "polygon", "faceColor": [0, 0, 0, 1]}], "axes": {}, "extent": {"zmax": 1.0, "ymax": 1.0, "zmin": 0.0, "xmax": 1.0, "xmin": 0.0, "ymin": 0.0}}
  // The nulls are the "scaled" parts of coordinates that
  // depend on the size of the final graphics (see Mathematica's Scaled). 

  // TODO: update the size of the container dynamically
  // (we also need some mechanism to update the enclosing <mspace>).

  // TODO: create axes using the (yet to be generated) information in data.axes.

  // TODO: colors, lighting/shading, handling of VertexNormals.

  var camera, scene, renderer, axes,
    isMouseDown = false, onMouseDownPosition, radius,
    tmpx, tmpy, tmpz, 
    theta = 45, onMouseDownTheta = 45, phi = 60, onMouseDownPhi = 60;

  // Center of the scene
  var center = new THREE.Vector3(
    0.5*(data.extent["xmin"] + data.extent["xmax"]),
    0.5*(data.extent["ymin"] + data.extent["ymax"]), 
    0.5*(data.extent["zmin"] + data.extent["zmax"]));

  // Where the camera is looking
  var focus = new THREE.Vector3(center.x, center.y, center.z);

  radius = 2*Math.sqrt(
   Math.pow(data.extent["xmax"]-data.extent["xmin"],2) +
   Math.pow(data.extent["ymax"]-data.extent["ymin"],2) +
   Math.pow(data.extent["zmax"]-data.extent["zmin"],2));

  // Scene
  scene = new THREE.Scene();
  scene.position = center;

  camera = new THREE.PerspectiveCamera(
    35,             // Field of view
    800 / 600,      // Aspect ratio
    0.1*radius,     // Near plane
    1000*radius     // Far plane
  );

  function update_camera_position() {
    camera.position.x = focus.x + radius * Math.sin(theta * Math.PI / 180) * Math.cos(phi * Math.PI / 180);
    camera.position.y = focus.y + radius * Math.cos(theta * Math.PI / 180) * Math.cos(phi * Math.PI / 180);
    camera.position.z = focus.z + radius * Math.sin(phi * Math.PI / 180);
    camera.lookAt(focus);
  }

  update_camera_position();
  camera.up = new THREE.Vector3(0,0,1);

  scene.add(camera);

  // Axes
  axes = new THREE.Mesh(
    new THREE.CubeGeometry(
      data.extent["xmax"]-data.extent["xmin"],
      data.extent["ymax"]-data.extent["ymin"],
      data.extent["zmax"]-data.extent["zmin"]),
    new THREE.MeshLambertMaterial({color: 0x000000, wireframe: true})
  );
  axes.position = center;
  scene.add(axes);  

  // Plot the primatives
  for (var indx = 0; indx < data.elements.length; indx++) {
    var type = data.elements[indx].type;
    switch(type) {
      case "point":
        scene.add(drawPoint(data.elements[indx]));
        break
      case "line":
        scene.add(drawLine(data.elements[indx]));
        break;
      case "polygon":
        scene.add(drawPolygon(data.elements[indx]));
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
    renderer.render( scene, camera );
  };

  function toScreenXY(position) {
    var camz = new THREE.Vector3(
        focus.x - camera.position.x,
        focus.y - camera.position.y,
        focus.z - camera.position.z
    );
    camz.normalize();

    var camx = new THREE.Vector3(
        radius * Math.cos(phi * Math.PI / 180) * Math.cos(theta * Math.PI / 180),
        - radius * Math.cos(phi * Math.PI / 180) * Math.sin(theta * Math.PI / 180),
        0
    );
    camx.normalize();

    var camy = new THREE.Vector3();
    camy.cross(camz, camx);

    var campos = new THREE.Vector3(
        position.x - camera.position.x + focus.x,
        position.y - camera.position.y + focus.y,
        position.z - camera.position.z + focus.z
    );

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
      proj2d = toScreenXY(axes.geometry.vertices[i]);

      angle = 57.296 * Math.max(
         Math.abs(Math.atan(proj2d.x/proj2d.z)),
         Math.abs(Math.atan(proj2d.y/proj2d.z))
      );
      tmp_fov = Math.max(tmp_fov, 2*angle);
    }

    camera.fov = tmp_fov;
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

    onMouseDownFocus = new THREE.Vector3(focus.x, focus.y, focus.z);
  }

  function onDocumentMouseMove(event) {
    event.preventDefault();

    if (isMouseDown) {
      if (event.shiftKey) {
        // console.log("Pan");
        if (! isShiftDown) {
          isShiftDown = true;
          onMouseDownPosition.x = event.clientX;
          onMouseDownPosition.y = event.clientY;
          autoRescale = false;
          container.style.cursor = "move";
        }
        var camz = new THREE.Vector3(
            focus.x - camera.position.x,
            focus.y - camera.position.y,
            focus.z - camera.position.z
        );
        camz.normalize();

        var camx = new THREE.Vector3(
            radius * Math.cos(phi * Math.PI / 180) * Math.cos(theta * Math.PI / 180),
            - radius * Math.cos(phi * Math.PI / 180) * Math.sin(theta * Math.PI / 180),
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

        theta = (event.clientX - onMouseDownPosition.x) + onMouseDownTheta;
        phi = (event.clientY - onMouseDownPosition.y) + onMouseDownPhi;
        phi = Math.max(Math.min(90, phi),-90);

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
  }

  // Bind Mouse events
  container.addEventListener('mousemove', onDocumentMouseMove, false);
  container.addEventListener('mousedown', onDocumentMouseDown, false);
  container.addEventListener('mouseup', onDocumentMouseUp, false);
  onMouseDownPosition = new THREE.Vector2();
  autoRescale = true;

  update_camera_position();
  ScaleInView();
  render();
}

