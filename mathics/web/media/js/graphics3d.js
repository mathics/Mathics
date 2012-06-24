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
	
	var camera, scene, renderer, uplane, lplane, gplane,
		isMouseDown = false, onMouseDownPosition, radius = 2,
		theta = 45, onMouseDownTheta = 45, phi = 60, onMouseDownPhi = 60;

	var numx = 25, numz = 25;
	
	renderer = new THREE.WebGLRenderer({antialias: true});
  renderer.setSize(400, 400);
  
  container.appendChild(renderer.domElement);

  scene = new THREE.Scene();

  // Camera
  camera = new THREE.PerspectiveCamera(
                                  35,             // Field of view
                                  800 / 600,      // Aspect ratio
                                  0.1,            // Near plane
                                  10000           // Far plane
                              );

  camera.position.x = radius * Math.sin(theta * Math.PI / 360) * Math.cos(phi * Math.PI / 360);
  camera.position.y = radius * Math.sin(phi * Math.PI / 360);
  camera.position.z = radius * Math.cos(theta * Math.PI / 360) * Math.cos(phi * Math.PI / 360);
  camera.lookAt(scene.position);
  scene.add(camera);

  // Lights
  var light = new THREE.PointLight(0xFFFFFF);
  light.position.set(0, -1000, 0);
  scene.add(light);

  var light = new THREE.PointLight(0xFFFFFF);
  light.position.set(0, 1000, 0);
  scene.add(light);

  // Plot Surfaces
  uplane = new THREE.Mesh(        // Upper Surface
          new THREE.PlaneGeometry(1, 1, numx, numz),
          new THREE.MeshLambertMaterial({color: 0xCC0000})
  );

  lplane = new THREE.Mesh(        // Lower Surface
          new THREE.PlaneGeometry(1, 1, numx, numz),
          new THREE.MeshLambertMaterial({color: 0x0000CC})
  );
  lplane.flipSided = true;

  gplane = new THREE.Mesh(        // Grid Lines
          new THREE.PlaneGeometry(1, 1, numx, numz),
          new THREE.MeshLambertMaterial({color: 0x000000, wireframe: true})
  );

  var x, y, z, xi, zi;

  for (xi = 0; xi <= numx; xi++) {
    x = -0.5 + xi / (numx + 1.0);
    for (zi = 0; zi <= numz; zi++) {
      z = -0.5 + zi / (numz + 1.0);
      y = (x*z)/(x * x + z * z + 0.2);

      uplane.geometry.vertices[(numz + 1)*zi + xi].x = x;
      uplane.geometry.vertices[(numz + 1)*zi + xi].z = z;
      uplane.geometry.vertices[(numz + 1)*zi + xi].y = y;

      lplane.geometry.vertices[(numz + 1)*zi + xi].x = x;
      lplane.geometry.vertices[(numz + 1)*zi + xi].z = z;
      lplane.geometry.vertices[(numz + 1)*zi + xi].y = y;

      gplane.geometry.vertices[(numz + 1)*zi + xi].x = x;
      gplane.geometry.vertices[(numz + 1)*zi + xi].z = z;
      gplane.geometry.vertices[(numz + 1)*zi + xi].y = y;
    }
  }
  scene.add(uplane);
  scene.add(lplane);
  scene.add(gplane);

  // Axes
  var axes = new THREE.Mesh(
                              new THREE.CubeGeometry(1.0,1.0,1.0),
                              new THREE.MeshLambertMaterial({color: 0x000000, wireframe: true})
  );
  scene.add(axes);  

	function render() {
	    renderer.render( scene, camera );
	};
	
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
      theta = -((event.clientX - onMouseDownPosition.x) * 0.5) + onMouseDownTheta;
      phi = ((event.clientY - onMouseDownPosition.y) * 0.5 ) + onMouseDownPhi;

      phi = Math.min(180, phi);

      camera.position.x = radius * Math.sin(theta * Math.PI / 360) * Math.cos(phi * Math.PI / 360);
      camera.position.y = radius * Math.sin(phi * Math.PI / 360);
      camera.position.z = radius * Math.cos(theta * Math.PI / 360) * Math.cos(phi * Math.PI / 360);
      camera.lookAt(scene.position);
      camera.updateMatrix();

      render();
    }
	}
	
	function onDocumentMouseUp(event) {
    event.preventDefault();

    isMouseDown = false;

    onMouseDownPosition.x = event.clientX - onMouseDownPosition.x;
    onMouseDownPosition.y = event.clientY - onMouseDownPosition.y;

    render();
	}
	
	function onDocumentMouseWheel( event ) {
    // event.preventDefault();

    //radius -= 0.002*event.wheelDeltaY;
    if (event.wheelDeltaY > 0) {
      radius *= 0.9;
    } else {
      radius /= 0.9;
    }

    camera.position.x = radius * Math.sin(theta * Math.PI / 360) * Math.cos(phi * Math.PI / 360);
    camera.position.y = radius * Math.sin(phi * Math.PI / 360);
    camera.position.z = radius * Math.cos(theta * Math.PI / 360) * Math.cos(phi * Math.PI / 360);
    camera.lookAt(scene.position);
    camera.updateMatrix();

    render();
	}

  // Bind Mouse events
  container.addEventListener('mousemove', onDocumentMouseMove, false);
  container.addEventListener('mousedown', onDocumentMouseDown, false);
  container.addEventListener('mouseup', onDocumentMouseUp, false);
  container.addEventListener('mousewheel', onDocumentMouseWheel, false);
  onMouseDownPosition = new THREE.Vector2();
  
  render();
}
