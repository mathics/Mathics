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
    
    var camera, scene, renderer, light, axes, tmpx, tmpy, tmpz,
        uplane, lplane, gplane, light, axes,
        isMouseDown = false, onMouseDownPosition, radius = 2,
        theta = 45, onMouseDownTheta = 45, phi = 60, onMouseDownPhi = 60;

    var numx = 25, numz = 25;
    
    renderer = new THREE.WebGLRenderer({antialias: true});
    renderer.setSize(800, 600);
     
    container.appendChild(renderer.domElement);

    scene = new THREE.Scene();

    // Camera
    camera = new THREE.OrthographicCamera(-0.65, 0.65, 0.65, -0.65, 1, 10);
    camera.position.x = radius * Math.sin(theta * Math.PI / 360) * Math.cos(phi * Math.PI / 360);
    camera.position.y = radius * Math.sin(phi * Math.PI / 360);
    camera.position.z = radius * Math.cos(theta * Math.PI / 360) * Math.cos(phi * Math.PI / 360);
    camera.lookAt(scene.position);
    scene.add(camera);

    // Lighting
    light = new THREE.PointLight(0xFFFFFF);
    light.position.set(0.0, 1.0, 0.0);
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

    var xi, zi;

    for (xi = 0; xi <= numx; xi++) {
        tmpx = -0.5 + xi / numx;
        for (zi = 0; zi <= numz; zi++) {
            tmpz = -0.5 + zi / numz;
            tmpy = (tmpx * tmpz) / (tmpx * tmpx + tmpz * tmpz + 0.2);

            uplane.geometry.vertices[(numz + 1)*zi + xi].x = tmpx;
            uplane.geometry.vertices[(numz + 1)*zi + xi].z = tmpz;
            uplane.geometry.vertices[(numz + 1)*zi + xi].y = tmpy;

            lplane.geometry.vertices[(numz + 1)*zi + xi].x = tmpx;
            lplane.geometry.vertices[(numz + 1)*zi + xi].z = tmpz;
            lplane.geometry.vertices[(numz + 1)*zi + xi].y = tmpy;

            gplane.geometry.vertices[(numz + 1)*zi + xi].x = tmpx;
            gplane.geometry.vertices[(numz + 1)*zi + xi].z = tmpz;
            gplane.geometry.vertices[(numz + 1)*zi + xi].y = tmpy;
        }
    }
    scene.add(uplane);
    scene.add(lplane);
    scene.add(gplane);

    // Axes
    axes = new THREE.Mesh(
            new THREE.CubeGeometry(1.0,1.0,1.0),
            new THREE.MeshLambertMaterial({color: 0x000000, wireframe: true})
    );
    scene.add(axes);  

    function render() {
        renderer.render( scene, camera );
    };
    
    // Automatic Rescaling
    function ScaleInView() { 
        camera.left = 0.0;
        camera.right = 0.0;
        camera.bottom = 0.0;
        camera.top = 0.0;

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

            camera.left = Math.min(tmpx, camera.left);
            camera.right = Math.max(tmpx, camera.right);
            camera.bottom = Math.min(tmpy, camera.bottom);
            camera.top = Math.max(tmpy, camera.top);
        }

        // Keep aspect ratio
        camera.right = Math.max(camera.right, -camera.left, camera.top, -camera.bottom);
        camera.left = Math.min(-camera.right, camera.left, -camera.top, camera.bottom);
        camera.top = Math.max(camera.right, -camera.left, camera.top, -camera.bottom);
        camera.bottom = Math.min(-camera.right, camera.left, -camera.top, camera.bottom);
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
            theta = -((event.clientX - onMouseDownPosition.x) * 0.5) + onMouseDownTheta;
            phi = ((event.clientY - onMouseDownPosition.y) * 0.5 ) + onMouseDownPhi;

            phi = Math.max(Math.min(180, phi),-180);

            camera.position.x = radius * Math.sin(theta * Math.PI / 360) * Math.cos(phi * Math.PI / 360);
            camera.position.y = radius * Math.sin(phi * Math.PI / 360);
            camera.position.z = radius * Math.cos(theta * Math.PI / 360) * Math.cos(phi * Math.PI / 360);
            camera.lookAt(scene.position);
            camera.updateProjectionMatrix();

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

        if (camera.right < 0 || camera.top < 0) {
            
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
