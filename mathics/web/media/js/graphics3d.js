function drawPlot3D(prim) {
  var mesh, plane, materials;
  var numx = 20, numz = 20;

  // console.log("drawPlot3D");
    
  plane = new THREE.PlaneGeometry(1, 1, numx, numz);

  var xi, zi;

  for (xi = 0; xi <= numx; xi++) {
    tmpx = -0.5 + xi / numx;
    for (zi = 0; zi <= numz; zi++) {
      tmpz = -0.5 + zi / numz;
      tmpy = (tmpx * tmpz) / (tmpx * tmpx + tmpz * tmpz + 0.2);

      plane.vertices[(numz + 1)*zi + xi].x = tmpx;
      plane.vertices[(numz + 1)*zi + xi].z = tmpz;
      plane.vertices[(numz + 1)*zi + xi].y = tmpy;
    }
  }
  plane.computeFaceNormals();

  materials = new Array(
    new THREE.MeshNormalMaterial( { overdraw: true } ),
    new THREE.MeshLambertMaterial( {color: 0x000000, wireframe: true} )
  );

  mesh = new THREE.SceneUtils.createMultiMaterialObject(plane, materials);
  mesh.children[0].doubleSided = true;

  // These 3 lines put the grid on the right side of the surface
  mesh.children[0].material.polygonOffset = true;
  mesh.children[0].material.polygonOffsetFactor = 1;
  mesh.children[0].material.polygonOffsetUnits = 1;

  return mesh;
}

function drawPolygon(prim) {    
  //TODO: Handle arbitary polygons. For example, S-shapes, shapes with holes.

  var mesh, poly, material;

  // console.log("drawPolygon");

  poly = new THREE.Geometry();
  var center = new THREE.Vector3(0,0,0);

  for (var i = 0; i < prim.coords.length; i++) {
    var tmpv = prim.coords[i][0];
    poly.vertices.push(new THREE.Vector3(tmpv[0], tmpv[1], tmpv[2]));
    center.x += tmpv[0] * (1.0 / prim.coords.length);
    center.y += tmpv[1] * (1.0 / prim.coords.length);
    center.z += tmpv[2] * (1.0 / prim.coords.length);
  }
  poly.vertices.push(center);

  for (var i = 0; i < prim.coords.length-1; i++) {
    poly.faces.push(new THREE.Face3(prim.coords.length,i,i+1));
  }
  poly.faces.push(new THREE.Face3(prim.coords.length,prim.coords.length-1,0));

  mesh = new THREE.Mesh(poly, new THREE.MeshNormalMaterial());
  mesh.doubleSided = true;

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

  // We just create a sample plot for now.

  var camera, scene, renderer, axes,
    isMouseDown = false, onMouseDownPosition, radius,
    tmpx, tmpy, tmpz, 
    theta = 45, onMouseDownTheta = 45, phi = 60, onMouseDownPhi = 60;

  var center = new THREE.Vector3(
    0.5*(data.extent["xmin"] + data.extent["xmax"]),
    0.5*(data.extent["ymin"] + data.extent["ymax"]), 
    0.5*(data.extent["zmin"] + data.extent["zmax"]));

  radius = 2*Math.sqrt(
   Math.pow(data.extent["xmax"]-data.extent["xmin"],2) +
   Math.pow(data.extent["ymax"]-data.extent["ymin"],2) +
   Math.pow(data.extent["zmax"]-data.extent["zmin"],2));

  // Scene
  scene = new THREE.Scene();
  scene.position = center;

  // Camera - TODO: Handle Different choices
  /*
  camera = new THREE.OrthographicCamera(
    data.extent["xmin"], data.extent["xmax"],
    data.extent["ymin"], data.extent["ymax"],
    data.extent["zmin"], data.extent["zmax"]
  );
  */

  camera = new THREE.PerspectiveCamera(
    35,             // Field of view
    800 / 600,      // Aspect ratio
    0.1*radius,     // Near plane
    1000*radius     // Far plane
  );

  camera.position.x = center.x + radius * Math.sin(theta * Math.PI / 360) * Math.cos(phi * Math.PI / 360);
  camera.position.y = center.y + radius * Math.sin(phi * Math.PI / 360);
  camera.position.z = center.z + radius * Math.cos(theta * Math.PI / 360) * Math.cos(phi * Math.PI / 360);
  camera.lookAt(scene.position);
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
      case "polygon":
        scene.add(drawPolygon(data.elements[indx]));
        break;
      case "plot3d":
        scene.add(drawPlot3D(data.elements[indx]));
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

  // Automatic Rescaling
  function ScaleInView() { 
    if (camera instanceof THREE.OrthographicCamera) {
      camera.left = 0.0;
      camera.right = 0.0;
      camera.bottom = 0.0;
      camera.top = 0.0;
    } else if (camera instanceof THREE.PerspectiveCamera) {
      camera.fov = 0.0;
    }

    var tmp_theta, tmp_phi;

    for (i=0; i<=7; i++) { // Check each index and adjust camera bounds
      tmp_theta = 0.25*Math.PI*(2.*i + 1) - theta * Math.PI / 360;

      if (i <= 3) {
        tmp_phi = 0.25*Math.PI - phi * Math.PI / 360;
      } else {
        tmp_phi = -0.25*Math.PI - phi * Math.PI / 360;
      }

      tmpx = Math.cos(tmp_theta) * Math.sin(tmp_phi)
      tmpy = Math.sin(tmp_theta) * Math.sin(tmp_phi);

      if (camera instanceof THREE.OrthographicCamera) {
        camera.left = Math.min(tmpx, camera.left);
        camera.right = Math.max(tmpx, camera.right);
        camera.bottom = Math.min(tmpy, camera.bottom);
        camera.top = Math.max(tmpy, camera.top);
      } else if (camera instanceof THREE.PerspectiveCamera) {

        var vec1 = new THREE.Vector3( axes.position.x - camera.position.x, 
                                      axes.position.y - camera.position.y,
                                      axes.position.z - camera.position.z
        );
        var vec2 = new THREE.Vector3( axes.geometry.vertices[i].x - camera.position.x,
                                      axes.geometry.vertices[i].y - camera.position.y,
                                      axes.geometry.vertices[i].z - camera.position.z
        );

        var angle = 57.296 * Math.acos(vec1.dot(vec2) / (vec1.length() * vec2.length()));
        
        camera.fov = Math.max(camera.fov, 2*angle);
      }
    }

    // Keep aspect ratio
    if (camera instanceof THREE.OrthographicCamera) {
      camera.right = Math.max(camera.right, -camera.left, camera.top, -camera.bottom);
      camera.left = Math.min(-camera.right, camera.left, -camera.top, camera.bottom);
      camera.top = Math.max(camera.right, -camera.left, camera.top, -camera.bottom);
      camera.bottom = Math.min(-camera.right, camera.left, -camera.top, camera.bottom);
    } else if (camera instanceof THREE.PerspectiveCamera) {
      camera.fov += 1.0;
    }
    camera.updateProjectionMatrix();
  }

  // Mouse Interactions
  function onDocumentMouseDown( event ) {
    event.preventDefault();

    isMouseDown = true;

    onMouseDownTheta = theta;
    onMouseDownPhi = phi;
    onMouseDownPosition.x = event.clientX;
    onMouseDownPosition.y = event.clientY;
  }

  function onDocumentMouseMove(event) {
    event.preventDefault();

    if (isMouseDown) {
      theta = -(event.clientX - onMouseDownPosition.x) + onMouseDownTheta;
      phi = (event.clientY - onMouseDownPosition.y) + onMouseDownPhi;

      phi = Math.max(Math.min(180, phi),-180);

      camera.position.x = center.x + radius * Math.sin(theta * Math.PI / 360) * Math.cos(phi * Math.PI / 360);
      camera.position.y = center.y + radius * Math.sin(phi * Math.PI / 360);
      camera.position.z = center.z + radius * Math.cos(theta * Math.PI / 360) * Math.cos(phi * Math.PI / 360);
      camera.lookAt(scene.position);

      if (camera instanceof THREE.OrthographicCamera) {
        camera.updateProjectionMatrix();
      } else if (camera instanceof THREE.PerspectiveCamera) {
        camera.updateMatrix();
      }

      render();
     }
  }

  function onDocumentMouseUp(event) {
    event.preventDefault();

    isMouseDown = false;

    onMouseDownPosition.x = event.clientX - onMouseDownPosition.x;
    onMouseDownPosition.y = event.clientY - onMouseDownPosition.y;

    ScaleInView();
    render();
  }

  function onDocumentMouseWheel( event ) {
    if (camera instanceof THREE.OrthographicCamera) {
      if (event.wheelDeltaY > 0) {
        camera.left += 0.1;
        camera.right -= 0.1;
        camera.bottom += 0.1;
        camera.top -= 0.1;
      } else {
        camera.left -= 0.1;
        camera.right += 0.1;
        camera.bottom -= 0.1;
        camera.top += 0.1;
      }

      if (camera.right <= 0 || camera.top <= 0) {
        camera.right = 0.1;
        camera.left = -0.1;
        camera.top = 0.1;
        camera.bottom = -0.1;
      }
    } else if (camera instanceof THREE.PerspectiveCamera) {
      if (event.wheelDeltaY > 0) {
        camera.fov *= 0.8;
      } else {
        camera.fov /= 0.8;
      }
      camera.fov = Math.max(1, Math.min(camera.fov, 150));
    }
    camera.updateProjectionMatrix();
    render();
  }

  // Bind Mouse events
  container.addEventListener('mousemove', onDocumentMouseMove, false);
  container.addEventListener('mousedown', onDocumentMouseDown, false);
  container.addEventListener('mouseup', onDocumentMouseUp, false);
  container.addEventListener('mousewheel', onDocumentMouseWheel, false);
  onMouseDownPosition = new THREE.Vector2();

  ScaleInView();
  render();
}

