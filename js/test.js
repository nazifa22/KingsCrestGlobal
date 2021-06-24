// var BufferGeometryUtils = {

// 	computeTangents: function ( geometry ) {

// 		var index = geometry.index;
// 		var attributes = geometry.attributes;

// 		// based on http://www.terathon.com/code/tangent.html
// 		// (per vertex tangents)

// 		if ( index === null ||
// 			 attributes.position === undefined ||
// 			 attributes.normal === undefined ||
// 			 attributes.uv === undefined ) {

// 			console.warn( 'THREE.BufferGeometry: Missing required attributes (index, position, normal or uv) in BufferGeometry.computeTangents()' );
// 			return;

// 		}

// 		var indices = index.array;
// 		var positions = attributes.position.array;
// 		var normals = attributes.normal.array;
// 		var uvs = attributes.uv.array;

// 		var nVertices = positions.length / 3;

// 		if ( attributes.tangent === undefined ) {

// 			geometry.setAttribute( 'tangent', new BufferAttribute( new Float32Array( 4 * nVertices ), 4 ) );

// 		}

// 		var tangents = attributes.tangent.array;

// 		var tan1 = [], tan2 = [];

// 		for ( var i = 0; i < nVertices; i ++ ) {

// 			tan1[ i ] = new Vector3();
// 			tan2[ i ] = new Vector3();

// 		}

// 		var vA = new Vector3(),
// 			vB = new Vector3(),
// 			vC = new Vector3(),

// 			uvA = new Vector2(),
// 			uvB = new Vector2(),
// 			uvC = new Vector2(),

// 			sdir = new Vector3(),
// 			tdir = new Vector3();

// 		function handleTriangle( a, b, c ) {
// 			vA.fromArray( positions, a * 3 );
// 			vB.fromArray( positions, b * 3 );
// 			vC.fromArray( positions, c * 3 );
// 			uvA.fromArray( uvs, a * 2 );
// 			uvB.fromArray( uvs, b * 2 );
// 			uvC.fromArray( uvs, c * 2 );
// 			vB.sub( vA );
// 			vC.sub( vA );
// 			uvB.sub( uvA );
// 			uvC.sub( uvA );
// 			var r = 1.0 / ( uvB.x * uvC.y - uvC.x * uvB.y );

// 			if ( ! isFinite( r ) ) return;
// 			sdir.copy( vB ).multiplyScalar( uvC.y ).addScaledVector( vC, - uvB.y ).multiplyScalar( r );
// 			tdir.copy( vC ).multiplyScalar( uvB.x ).addScaledVector( vB, - uvC.x ).multiplyScalar( r );

// 			tan1[ a ].add( sdir );
// 			tan1[ b ].add( sdir );
// 			tan1[ c ].add( sdir );

// 			tan2[ a ].add( tdir );
// 			tan2[ b ].add( tdir );
// 			tan2[ c ].add( tdir );

// 		}

// 		var groups = geometry.groups;

// 		if ( groups.length === 0 ) {

// 			groups = [ {
// 				start: 0,
// 				count: indices.length
// 			} ];

// 		}

// 		for ( var i = 0, il = groups.length; i < il; ++ i ) {

// 			var group = groups[ i ];

// 			var start = group.start;
// 			var count = group.count;

// 			for ( var j = start, jl = start + count; j < jl; j += 3 ) {

// 				handleTriangle(
// 					indices[ j + 0 ],
// 					indices[ j + 1 ],
// 					indices[ j + 2 ]
// 				);

// 			}

// 		}

// 		var tmp = new Vector3(), tmp2 = new Vector3();
// 		var n = new Vector3(), n2 = new Vector3();
// 		var w, t, test;

// 		function handleVertex( v ) {

// 			n.fromArray( normals, v * 3 );
// 			n2.copy( n );

// 			t = tan1[ v ];

// 			// Gram-Schmidt orthogonalize

// 			tmp.copy( t );
// 			tmp.sub( n.multiplyScalar( n.dot( t ) ) ).normalize();

// 			// Calculate handedness

// 			tmp2.crossVectors( n2, t );
// 			test = tmp2.dot( tan2[ v ] );
// 			w = ( test < 0.0 ) ? - 1.0 : 1.0;

// 			tangents[ v * 4 ] = tmp.x;
// 			tangents[ v * 4 + 1 ] = tmp.y;
// 			tangents[ v * 4 + 2 ] = tmp.z;
// 			tangents[ v * 4 + 3 ] = w;

// 		}

// 		for ( var i = 0, il = groups.length; i < il; ++ i ) {

// 			var group = groups[ i ];

// 			var start = group.start;
// 			var count = group.count;

// 			for ( var j = start, jl = start + count; j < jl; j += 3 ) {

// 				handleVertex( indices[ j + 0 ] );
// 				handleVertex( indices[ j + 1 ] );
// 				handleVertex( indices[ j + 2 ] );

// 			}

// 		}

// 	},

// 	mergeBufferGeometries: function ( geometries, useGroups ) {

// 		var isIndexed = geometries[ 0 ].index !== null;

// 		var attributesUsed = new Set( Object.keys( geometries[ 0 ].attributes ) );
// 		var morphAttributesUsed = new Set( Object.keys( geometries[ 0 ].morphAttributes ) );

// 		var attributes = {};
// 		var morphAttributes = {};

// 		var morphTargetsRelative = geometries[ 0 ].morphTargetsRelative;

// 		var mergedGeometry = new BufferGeometry();

// 		var offset = 0;

// 		for ( var i = 0; i < geometries.length; ++ i ) {

// 			var geometry = geometries[ i ];

// 			// ensure that all geometries are indexed, or none

// 			if ( isIndexed !== ( geometry.index !== null ) ) return null;

// 			// gather attributes, exit early if they're different

// 			for ( var name in geometry.attributes ) {

// 				if ( ! attributesUsed.has( name ) ) return null;

// 				if ( attributes[ name ] === undefined ) attributes[ name ] = [];

// 				attributes[ name ].push( geometry.attributes[ name ] );

// 			}

// 			// gather morph attributes, exit early if they're different

// 			if ( morphTargetsRelative !== geometry.morphTargetsRelative ) return null;

// 			for ( var name in geometry.morphAttributes ) {

// 				if ( ! morphAttributesUsed.has( name ) ) return null;

// 				if ( morphAttributes[ name ] === undefined ) morphAttributes[ name ] = [];

// 				morphAttributes[ name ].push( geometry.morphAttributes[ name ] );

// 			}

// 			// gather .userData

// 			mergedGeometry.userData.mergedUserData = mergedGeometry.userData.mergedUserData || [];
// 			mergedGeometry.userData.mergedUserData.push( geometry.userData );

// 			if ( useGroups ) {

// 				var count;

// 				if ( isIndexed ) {

// 					count = geometry.index.count;

// 				} else if ( geometry.attributes.position !== undefined ) {

// 					count = geometry.attributes.position.count;

// 				} else {

// 					return null;

// 				}

// 				mergedGeometry.addGroup( offset, count, i );

// 				offset += count;

// 			}

// 		}

// 		// merge indices

// 		if ( isIndexed ) {

// 			var indexOffset = 0;
// 			var mergedIndex = [];

// 			for ( var i = 0; i < geometries.length; ++ i ) {

// 				var index = geometries[ i ].index;

// 				for ( var j = 0; j < index.count; ++ j ) {

// 					mergedIndex.push( index.getX( j ) + indexOffset );

// 				}

// 				indexOffset += geometries[ i ].attributes.position.count;

// 			}

// 			mergedGeometry.setIndex( mergedIndex );

// 		}

// 		// merge attributes

// 		for ( var name in attributes ) {

// 			var mergedAttribute = this.mergeBufferAttributes( attributes[ name ] );

// 			if ( ! mergedAttribute ) return null;

// 			mergedGeometry.setAttribute( name, mergedAttribute );

// 		}

// 		// merge morph attributes

// 		for ( var name in morphAttributes ) {

// 			var numMorphTargets = morphAttributes[ name ][ 0 ].length;

// 			if ( numMorphTargets === 0 ) break;

// 			mergedGeometry.morphAttributes = mergedGeometry.morphAttributes || {};
// 			mergedGeometry.morphAttributes[ name ] = [];

// 			for ( var i = 0; i < numMorphTargets; ++ i ) {

// 				var morphAttributesToMerge = [];

// 				for ( var j = 0; j < morphAttributes[ name ].length; ++ j ) {

// 					morphAttributesToMerge.push( morphAttributes[ name ][ j ][ i ] );

// 				}

// 				var mergedMorphAttribute = this.mergeBufferAttributes( morphAttributesToMerge );

// 				if ( ! mergedMorphAttribute ) return null;

// 				mergedGeometry.morphAttributes[ name ].push( mergedMorphAttribute );

// 			}

// 		}

// 		return mergedGeometry;

// 	},

// 	mergeBufferAttributes: function ( attributes ) {

// 		var TypedArray;
// 		var itemSize;
// 		var normalized;
// 		var arrayLength = 0;

// 		for ( var i = 0; i < attributes.length; ++ i ) {

// 			var attribute = attributes[ i ];

// 			if ( attribute.isInterleavedBufferAttribute ) return null;

// 			if ( TypedArray === undefined ) TypedArray = attribute.array.constructor;
// 			if ( TypedArray !== attribute.array.constructor ) return null;

// 			if ( itemSize === undefined ) itemSize = attribute.itemSize;
// 			if ( itemSize !== attribute.itemSize ) return null;

// 			if ( normalized === undefined ) normalized = attribute.normalized;
// 			if ( normalized !== attribute.normalized ) return null;

// 			arrayLength += attribute.array.length;

// 		}

// 		var array = new TypedArray( arrayLength );
// 		var offset = 0;

// 		for ( var i = 0; i < attributes.length; ++ i ) {

// 			array.set( attributes[ i ].array, offset );

// 			offset += attributes[ i ].array.length;

// 		}

// 		return new BufferAttribute( array, itemSize, normalized );

// 	},

	
// 	interleaveAttributes: function ( attributes ) {

// 		// Interleaves the provided attributes into an InterleavedBuffer and returns
// 		// a set of InterleavedBufferAttributes for each attribute
// 		var TypedArray;
// 		var arrayLength = 0;
// 		var stride = 0;

// 		// calculate the the length and type of the interleavedBuffer
// 		for ( var i = 0, l = attributes.length; i < l; ++ i ) {

// 			var attribute = attributes[ i ];

// 			if ( TypedArray === undefined ) TypedArray = attribute.array.constructor;
// 			if ( TypedArray !== attribute.array.constructor ) {

// 				console.warn( 'AttributeBuffers of different types cannot be interleaved' );
// 				return null;

// 			}

// 			arrayLength += attribute.array.length;
// 			stride += attribute.itemSize;

// 		}

// 		// Create the set of buffer attributes
// 		var interleavedBuffer = new InterleavedBuffer( new TypedArray( arrayLength ), stride );
// 		var offset = 0;
// 		var res = [];
// 		var getters = [ 'getX', 'getY', 'getZ', 'getW' ];
// 		var setters = [ 'setX', 'setY', 'setZ', 'setW' ];

// 		for ( var j = 0, l = attributes.length; j < l; j ++ ) {

// 			var attribute = attributes[ j ];
// 			var itemSize = attribute.itemSize;
// 			var count = attribute.count;
// 			var iba = new InterleavedBufferAttribute( interleavedBuffer, itemSize, offset, attribute.normalized );
// 			res.push( iba );

// 			offset += itemSize;

// 			// Move the data for each attribute into the new interleavedBuffer
// 			// at the appropriate offset
// 			for ( var c = 0; c < count; c ++ ) {

// 				for ( var k = 0; k < itemSize; k ++ ) {

// 					iba[ setters[ k ] ]( c, attribute[ getters[ k ] ]( c ) );

// 				}

// 			}

// 		}

// 		return res;

// 	},

	
// 	estimateBytesUsed: function ( geometry ) {

// 		// Return the estimated memory used by this geometry in bytes
// 		// Calculate using itemSize, count, and BYTES_PER_ELEMENT to account
// 		// for InterleavedBufferAttributes.
// 		var mem = 0;
// 		for ( var name in geometry.attributes ) {

// 			var attr = geometry.getAttribute( name );
// 			mem += attr.count * attr.itemSize * attr.array.BYTES_PER_ELEMENT;

// 		}

// 		var indices = geometry.getIndex();
// 		mem += indices ? indices.count * indices.itemSize * indices.array.BYTES_PER_ELEMENT : 0;
// 		return mem;

// 	},

// 	mergeVertices: function ( geometry, tolerance = 1e-4 ) {

// 		tolerance = Math.max( tolerance, Number.EPSILON );

// 		// Generate an index buffer if the geometry doesn't have one, or optimize it
// 		// if it's already available.
// 		var hashToIndex = {};
// 		var indices = geometry.getIndex();
// 		var positions = geometry.getAttribute( 'position' );
// 		var vertexCount = indices ? indices.count : positions.count;

// 		// next value for triangle indices
// 		var nextIndex = 0;

// 		// attributes and new attribute arrays
// 		var attributeNames = Object.keys( geometry.attributes );
// 		var attrArrays = {};
// 		var morphAttrsArrays = {};
// 		var newIndices = [];
// 		var getters = [ 'getX', 'getY', 'getZ', 'getW' ];

// 		// initialize the arrays
// 		for ( var i = 0, l = attributeNames.length; i < l; i ++ ) {

// 			var name = attributeNames[ i ];

// 			attrArrays[ name ] = [];

// 			var morphAttr = geometry.morphAttributes[ name ];
// 			if ( morphAttr ) {

// 				morphAttrsArrays[ name ] = new Array( morphAttr.length ).fill().map( () => [] );

// 			}

// 		}

// 		// convert the error tolerance to an amount of decimal places to truncate to
// 		var decimalShift = Math.log10( 1 / tolerance );
// 		var shiftMultiplier = Math.pow( 10, decimalShift );
// 		for ( var i = 0; i < vertexCount; i ++ ) {

// 			var index = indices ? indices.getX( i ) : i;

// 			// Generate a hash for the vertex attributes at the current index 'i'
// 			var hash = '';
// 			for ( var j = 0, l = attributeNames.length; j < l; j ++ ) {

// 				var name = attributeNames[ j ];
// 				var attribute = geometry.getAttribute( name );
// 				var itemSize = attribute.itemSize;

// 				for ( var k = 0; k < itemSize; k ++ ) {

// 					// double tilde truncates the decimal value
// 					hash += `${ ~ ~ ( attribute[ getters[ k ] ]( index ) * shiftMultiplier ) },`;

// 				}

// 			}

// 			// Add another reference to the vertex if it's already
// 			// used by another index
// 			if ( hash in hashToIndex ) {

// 				newIndices.push( hashToIndex[ hash ] );

// 			} else {

// 				// copy data to the new index in the attribute arrays
// 				for ( var j = 0, l = attributeNames.length; j < l; j ++ ) {

// 					var name = attributeNames[ j ];
// 					var attribute = geometry.getAttribute( name );
// 					var morphAttr = geometry.morphAttributes[ name ];
// 					var itemSize = attribute.itemSize;
// 					var newarray = attrArrays[ name ];
// 					var newMorphArrays = morphAttrsArrays[ name ];

// 					for ( var k = 0; k < itemSize; k ++ ) {

// 						var getterFunc = getters[ k ];
// 						newarray.push( attribute[ getterFunc ]( index ) );

// 						if ( morphAttr ) {

// 							for ( var m = 0, ml = morphAttr.length; m < ml; m ++ ) {

// 								newMorphArrays[ m ].push( morphAttr[ m ][ getterFunc ]( index ) );

// 							}

// 						}

// 					}

// 				}

// 				hashToIndex[ hash ] = nextIndex;
// 				newIndices.push( nextIndex );
// 				nextIndex ++;

// 			}

// 		}

// 		// Generate typed arrays from new attribute arrays and update
// 		// the attributeBuffers
// 		const result = geometry.clone();
// 		for ( var i = 0, l = attributeNames.length; i < l; i ++ ) {

// 			var name = attributeNames[ i ];
// 			var oldAttribute = geometry.getAttribute( name );

// 			var buffer = new oldAttribute.array.constructor( attrArrays[ name ] );
// 			var attribute = new BufferAttribute( buffer, oldAttribute.itemSize, oldAttribute.normalized );

// 			result.setAttribute( name, attribute );

// 			// Update the attribute arrays
// 			if ( name in morphAttrsArrays ) {

// 				for ( var j = 0; j < morphAttrsArrays[ name ].length; j ++ ) {

// 					var oldMorphAttribute = geometry.morphAttributes[ name ][ j ];

// 					var buffer = new oldMorphAttribute.array.constructor( morphAttrsArrays[ name ][ j ] );
// 					var morphAttribute = new BufferAttribute( buffer, oldMorphAttribute.itemSize, oldMorphAttribute.normalized );
// 					result.morphAttributes[ name ][ j ] = morphAttribute;

// 				}

// 			}

// 		}

// 		// indices

// 		result.setIndex( newIndices );

// 		return result;

// 	},

// 	toTrianglesDrawMode: function ( geometry, drawMode ) {

// 		if ( drawMode === TrianglesDrawMode ) {

// 			console.warn( 'THREE.BufferGeometryUtils.toTrianglesDrawMode(): Geometry already defined as triangles.' );
// 			return geometry;

// 		}

// 		if ( drawMode === TriangleFanDrawMode || drawMode === TriangleStripDrawMode ) {

// 			var index = geometry.getIndex();

// 			// generate index if not present

// 			if ( index === null ) {

// 				var indices = [];

// 				var position = geometry.getAttribute( 'position' );

// 				if ( position !== undefined ) {

// 					for ( var i = 0; i < position.count; i ++ ) {

// 						indices.push( i );

// 					}

// 					geometry.setIndex( indices );
// 					index = geometry.getIndex();

// 				} else {

// 					console.error( 'THREE.BufferGeometryUtils.toTrianglesDrawMode(): Undefined position attribute. Processing not possible.' );
// 					return geometry;

// 				}

// 			}

// 			//

// 			var numberOfTriangles = index.count - 2;
// 			var newIndices = [];

// 			if ( drawMode === TriangleFanDrawMode ) {

// 				// gl.TRIANGLE_FAN

// 				for ( var i = 1; i <= numberOfTriangles; i ++ ) {

// 					newIndices.push( index.getX( 0 ) );
// 					newIndices.push( index.getX( i ) );
// 					newIndices.push( index.getX( i + 1 ) );

// 				}

// 			} else {

// 				// gl.TRIANGLE_STRIP

// 				for ( var i = 0; i < numberOfTriangles; i ++ ) {

// 					if ( i % 2 === 0 ) {

// 						newIndices.push( index.getX( i ) );
// 						newIndices.push( index.getX( i + 1 ) );
// 						newIndices.push( index.getX( i + 2 ) );


// 					} else {

// 						newIndices.push( index.getX( i + 2 ) );
// 						newIndices.push( index.getX( i + 1 ) );
// 						newIndices.push( index.getX( i ) );

// 					}

// 				}

// 			}

// 			if ( ( newIndices.length / 3 ) !== numberOfTriangles ) {

// 				console.error( 'THREE.BufferGeometryUtils.toTrianglesDrawMode(): Unable to generate correct amount of triangles.' );

// 			}

// 			// build final geometry

// 			var newGeometry = geometry.clone();
// 			newGeometry.setIndex( newIndices );
// 			newGeometry.clearGroups();

// 			return newGeometry;

// 		} else {

// 			console.error( 'THREE.BufferGeometryUtils.toTrianglesDrawMode(): Unknown draw mode:', drawMode );
// 			return geometry;

// 		}

// 	}

// };

// !Button

const button = () => {
    gsap.config({
        nullTargetWarn: false
    });
    var $button = $('.button');
    if ($button.length) {
        $button.each(function() {
            mouseMagnetic(this);
        });
    }

    function mouseMagnetic(item) {

        var $item = $(item);

        $item.each(function() {

            var $self = $(this).find('.wrapper');
            var hover = false;
            var offsetHoverMax = $self.attr("offset-hover-max") || 0.7;
            var offsetHoverMin = $self.attr("offset-hover-min") || 0.5;

            var attachEventsListener = function() {
                $(window).on("mousemove", function(e) {
                    var hoverArea = hover ? offsetHoverMax : offsetHoverMin;

                    var cursor = {
                        x: e.clientX,
                        y: e.clientY
                    };

                    var width = $self.outerWidth();
                    var height = $self.outerHeight();

                    var offset = $self.offset();
                    var elPos = {
                        x: offset.left + width / 2,
                        y: (offset.top - $(window).scrollTop()) + height / 2
                    };

                    var x = cursor.x - elPos.x;
                    var y = cursor.y - elPos.y;

                    var dist = Math.sqrt(x * x + y * y);

                    var mutHover = false;

                    if (dist < width * hoverArea) {
                        mutHover = true;
                        if (!hover) {
                            hover = true;
                        }
                        onHover(x, y);
                    }

                    if (!mutHover && hover) {
                        onLeave();
                        hover = false;
                    }
                });
            };

            function onHover(x, y) {
                gsap.to($self, 1.5, {
                    x: x * 0.2,
                    y: y * 0.2,
                    rotation: x * 0.05,
                    ease: Power2.easeOut
                }, 0);
            };

            function onLeave() {
                gsap.to($self, 1.5, {
                    x: 0,
                    y: 0,
                    rotation: 0,
                    ease: Elastic.easeOut
                }, 0);
            };

            attachEventsListener();

        });
    };
}
button();
// !Button

// ? Earth
const earth = () => {
    var renderer = new THREE.WebGLRenderer({
        alpha: true,
        antialias: true
    });
    var clock = new THREE.Clock(true);

    var scene = null;
    var _width = window.innerWidth;
    var _height = window.innerHeight;
    var aspect = _width / _height;
    var camera = new THREE.PerspectiveCamera(45, aspect, 1, 1000);
    var earth = null;
    var earthUniforms = null;
    var atmosphereUniforms = null;
    var atmosphere = null;
    var amount = $('#canvas').data('people');
    var mapC, group;

    var earthVertexShader = `uniform vec3 lightDirection;

                                varying vec2 vUv;
                                varying vec3 vEyeDirectionEyeSpace;
                                varying vec3 vLightDirection;
                                attribute vec4 tangent;

                                // all in eye space
                                varying mat3 tbn;

                                void main(){

                                vUv = uv;
                                gl_Position = projectionMatrix * modelViewMatrix * vec4(position, 1.0);

                                vLightDirection = mat3(viewMatrix) * lightDirection; // should be computed outside of shader
                                vEyeDirectionEyeSpace = mat3(viewMatrix) * normalize(position - cameraPosition).xyz;

                                // normal mapping
                                vec3 t = normalize(tangent.xyz);
                                vec3 n = normalize(normal.xyz);
                                vec3 b = normalize(cross(t, n));

                                // everything in eye space
                                t = normalize(normalMatrix * t);
                                b = normalize(normalMatrix * b);
                                n = normalize(normalMatrix * n);

                                tbn = mat3(t, b, n);

                                }`;

    var earthFragmentShader = ` uniform sampler2D diffuseTexture;
                                uniform sampler2D diffuseNight;
                                uniform sampler2D specularMap;
                                uniform sampler2D cloudsMap;
                                uniform sampler2D normalMap;

                                varying vec2 vUv;
                                varying vec3 vEyeDirectionEyeSpace;
                                varying vec3 vLightDirection;

                                // tangent-bitangent-normal matrix
                                varying mat3 tbn;

                                void main(){

                                vec3 lightDir = normalize(vLightDirection);

                                vec3 n        = texture2D(normalMap, vUv).xyz * 2.0 - 1.0;
                                vec3 normal   = normalize(tbn * n);


                                // directional light
                                float lightIntensity  = dot(normal, lightDir);
                                float selectImage     = dot(tbn[2], lightDir);

                                gl_FragColor = texture2D(diffuseTexture, vUv) * selectImage + texture2D(diffuseNight, vUv) * (1.0-selectImage);

                                //gl_FragColor = vec4(vec3(0.5), 1.0 );
                                gl_FragColor *= (1.0 + 10.0*(lightIntensity - selectImage));

                                // specular
                                vec3 reflection = reflect(lightDir, normal);
                                float specPower = texture2D(specularMap, vUv).r;

                                float spec  = 10.2;
                                float gloss = 0.1 * texture2D(specularMap, vUv).a;

                                float specular  =  pow(clamp(dot(reflection, normalize(vEyeDirectionEyeSpace)), 0.0, 1.0), spec) * gloss;
                                gl_FragColor    = gl_FragColor + specular * vec4(0.26, 0.96, 0.99, 1);

                                // cloud colors + a small bump
                                vec4 cloudsColor = texture2D(cloudsMap, vUv) * vec4(1.0, 1.5, 1.2, 1.0);

                                vec4 cloudsShadow = texture2D(cloudsMap, vec2(vUv.x+ normal.x * 0.005, vUv.y + normal.y * 0.005));

                                if (cloudsColor.r < 0.1 && cloudsShadow.r > 0.1){
                                    gl_FragColor *= 0.75;
                                    cloudsShadow = vec4(0);
                                }

                                gl_FragColor = gl_FragColor * (vec4(1.2) - cloudsColor) + cloudsColor * (lightIntensity * 2.0);

                                    }`;

    const atmosphereVertexShader = `uniform vec3 earthCenter;
                                    uniform float earthRadius;
                                    uniform float atmosphereRadius;
                                    uniform vec3 lightDirection;

                                    varying float atmosphereThickness;
                                    varying vec3 vLightDirection;
                                    varying vec3 vNormalEyeSpace;


                                    void main(){
                                    gl_Position = projectionMatrix * modelViewMatrix * vec4(position, 1.0);

                                    vec3 positionW = (modelMatrix * vec4(position, 1.0)).xyz;

                                    vec3 vCameraEarth   = cameraPosition.xyz - earthCenter;
                                    vec3 vCameraVertex  = normalize(cameraPosition.xyz - positionW);

                                    float tca = dot(vCameraEarth,  vCameraVertex);

                                    if (tca < 0.0){
                                        // not intesect, looking in opposite direction
                                        atmosphereThickness = 0.0;
                                        return;
                                    }

                                    float dsq = dot(vCameraEarth, vCameraEarth) - tca * tca;
                                    float thc_sq_atmosphere = max(atmosphereRadius * atmosphereRadius - dsq, 0.0);
                                    float thc_sq_earth = max(earthRadius * earthRadius - dsq, 0.0);

                                    float thc_atmosphere = 2.0 * sqrt(thc_sq_atmosphere);
                                    float thc_earth = 2.0 * sqrt(max(0.0,thc_sq_earth));

                                    float thc           = (thc_atmosphere - thc_earth) * 0.09; // 0.01 - density factor
                                    atmosphereThickness = thc;

                                    // light calculation
                                    vLightDirection = mat3(viewMatrix) * lightDirection;
                                    vNormalEyeSpace = normalize(normalMatrix * normal);

                                    }`;

    const atmosphereFragShader = ` varying float atmosphereThickness;
                                    varying vec3 vLightDirection;
                                    varying vec3 vNormalEyeSpace;

                                    void main(){

                                    vec3 lightDir = normalize(vLightDirection);
                                    vec3 normal = normalize(vNormalEyeSpace);
                                    float lightIntensity = max(dot(normal, lightDir) * 1.5, -0.7);
                                    gl_FragColor = vec4( (vec3(57.0, 97.0, 162.0) / 256.0) * (1.0 + lightIntensity), atmosphereThickness);

                                    }`;

    async function init() {
        renderer.setSize(_width, _height);
        renderer.autoClear = false;
        const appendItem = renderer.domElement;
        document.getElementById("canvas").appendChild(appendItem);

        scene = await loadObject("assets/earth_and_water.json");
        scene.fog = new THREE.Fog(0x000000, 1500, 2100);

        const textureLoader = new THREE.TextureLoader();

        group = new THREE.Group();

        for (let a = 0; a < amount.length; a++) {

            const x = Math.random() - 0.5;
            const y = getRandomInt(-0.15, 0.15);
            const z = Math.random() - 0.5;


            if (amount[0].place == 'south-america') {} else if (amount[0].place == 'north-america') {} else if (amount[0].place == 'europe') {

            } else if (amount[0].place == 'asia') {

            } else if (amount[0].place == 'africa') {

            } else if (amount[0].place == 'oceania') {

            }

            let material;

            mapC = textureLoader.load(amount[a].thumb);

            material = new THREE.SpriteMaterial({
                map: mapC,
                color: 0xffffff,
                fog: true
            });

            const sprite = new THREE.Sprite(material);

            sprite.position.set(x, y, z);
            sprite.position.normalize();
            sprite.position.multiplyScalar(12.2);

            group.add(sprite);

        }

        scene.add(group);

        camera.position.set(5, 0, 44);

        earth = scene.getObjectByName("Earth");
        atmosphere = scene.getObjectByName("Atmosphere");

        // Mesh Configurations
        earth.receiveShadow = true;
        earth.castShadow = true;

        camera.lookAt(earth.position);

        fixMaterials().then(() => {
            renderer.autoClear = false;
            render();
        });
    }

    function resize() {
        _width = $(window).width();
        _height = $(window).height();

        renderer.setSize(_width, _height);

        console.log(_width)

        if (camera != null) {
            camera.aspect = _width / _height;
            camera.updateProjectionMatrix();
        }
    }

    function update(dt) {

        let lightPos = new THREE.Vector3(-10, 10, -3);
        let lightPosU = new THREE.Uniform(newVector(lightPos));

        earthUniforms.lightDirection = lightPosU;
        earth.rotation.y += 0.05 * dt;

        atmosphereUniforms.lightDirection = lightPosU;
        atmosphere.rotation.y += 0.05 * dt;

        for (let i = 0, l = group.children.length; i < l; i++) {

            const sprite = group.children[i];

            sprite.scale.set(2, 2, 1);

        }

        group.rotation.y += 0.05 * dt;
        // console.log('lightDir', lightPosU)
    }

    function render() {
        requestAnimationFrame(render);
        update(clock.getDelta())
        renderer.clear();
        renderer.render(scene, camera);
    }

    function newVector(v) {
        return new THREE.Vector3(v.x, v.y, v.z);
    }

    function getRandomInt(min, max) {
        return Math.floor(Math.random() * (max - min + 1)) + min;
    }

    async function loadTexture(texture) {
        let imgLoader = new THREE.TextureLoader();

        return new Promise((resolve, reject) => imgLoader.load(texture,

            function(tex) {
                tex.anisotropy = renderer.capabilities.getMaxAnisotropy();
                resolve(tex);
            }, null, reject))
    }

    async function fixMaterials() {
        atmosphereUniforms = {
            earthCenter: new THREE.Uniform(earth.position),
            earthRadius: new THREE.Uniform(10.0),
            atmosphereRadius: new THREE.Uniform(10.4),
        }

        earthUniforms = {
            diffuseTexture: {
                type: "t",
                value: await loadTexture("assets/earth/earth_diffuse.jpg")
            },
            diffuseNight: {
                type: "t",
                value: await loadTexture("assets/earth/earth_diffuse_night.jpg")
            },
            normalMap: {
                type: "t",
                value: await loadTexture("assets/earth/earth_normal_map.jpg")
            },
            specularMap: {
                type: "t",
                value: await loadTexture("assets/earth/earth_specular_map.png")
            },
            cloudsMap: {
                type: "t",
                value: await loadTexture("assets/earth/earth_diffuse_clouds.jpg")
            }
        }

        update(0);
        // BufferGeometryUtils.computeTangents(earth.geometry);
        earth.material = new THREE.ShaderMaterial({
            uniforms: earthUniforms,
            vertexShader: earthVertexShader,
            fragmentShader: earthFragmentShader,
            side: THREE.FrontSide
        });

        atmosphere.material = new THREE.ShaderMaterial({
            uniforms: atmosphereUniforms,
            vertexShader: atmosphereVertexShader,
            fragmentShader: atmosphereFragShader,
            blending: THREE.CustomBlending,
            blendEquation: THREE.AddEquation,
            blendSrc: THREE.SrcAlphaFactor,
            blendDst: THREE.OneMinusSrcAlphaFactor,
            side: THREE.FrontSide,
            transparent: true,
        });
    }

    async function loadObject(json) {
        let objLoader = new THREE.ObjectLoader();
        return new Promise((accept, reject) => objLoader.load(json, accept, null, reject));
    }

    return {
        init,
        resize,
        update,
        render,
        newVector,
        getRandomInt,
        loadTexture,
        fixMaterials,
        loadObject
    }
}
// console.log(earth())
earth();
// ? Earth

// ^ Footer
const footer = () => {
    // console.log('footer')
    var $footer = $('.footer');
    var $scrollup = $footer.find('.scrollup');
    var dHeight = null;
    var wHeight = null;

    if ($footer.length) {
        $footer.css({
            'height': wHeight
        });

        $scrollup.find('.button').on('click', function() {
            gsap.to(window, 1, {
                scrollTo: {
                    y: 0,
                    ease: 'Power3.easeOut'
                }
            });
        });

        wHeight = window.innerHeight;
        dHeight = $(document).height();

        $(window).on('scroll.footer', onScroll);
        $(window).on('resize', onResize);

        onScroll();
        onResize();
    }

    function onResize() {
        wHeight = window.innerHeight;
        dHeight = $(document).height();

        $footer.css({
            'height': window.innerHeight
        });

        console.log(wHeight, dHeight);
    }

    function onScroll() {

        wHeight = window.innerHeight;
        dHeight = $(document).height();

        var scrollTop = $(window).scrollTop();

        //Scroll no bottom da página
        if (scrollTop + wHeight >= dHeight - (wHeight / 2)) {
            $footer.addClass('motion-in-1');
        } else {
            $footer.removeClass('motion-in-1');
        }

        if (scrollTop + wHeight >= dHeight - (wHeight / 3)) {
            $footer.addClass('motion-in-2');
        } else {
            $footer.removeClass('motion-in-2');
        }

        if (scrollTop + wHeight >= dHeight) {
            $footer.addClass('motion-in-3');
        } else {
            $footer.removeClass('motion-in-3');
        }
    }
}
footer();
// ^ Footer

// * Form 
const form = () => {
    function init(_element) {

        $(_element).on('focusout', function() {
            if ($(this).find('input').val() == '') {
                $(this).removeClass('add-focus');
            }
    
            if ($(this).find('textarea').val() == '') {
                $(this).removeClass('add-focus');
            }
        })
    
        $(_element).on('focusin', function() {
            $(this).addClass('add-focus');
        })
    
    }
    // console.log('form')

    return { init }
}
form();
// * Form 

// & Header 
const header = () => {
    var $header = $('.header');
    var $menu = $('.menu');
    var $hmbrg = $header.find('.hmbrg');

    var _dataPage = $('main').data('page');
    var _headerH = $('.header').find('.logo').height() + (($('.header').height() - $('.header').find('.logo').height()) / 2);

    if ($header.length) {

        $header.find('.menu').css({
            'height': window.innerHeight
        });
        $hmbrg.on('click', function() {
            if ($(this).hasClass('active')) {
                hideMenu();

            } else {
                showMenu();
            }
        });

        $(window).on('scroll.header', onScroll);
        $(window).on('resize', onResize);
        onScroll();
    }

    function onResize() {
        $header.find('.menu').css({
            'height': window.innerHeight
        });
    }

    function onScroll() {

        var dHeight = $(document).height();
        var wHeight = window.innerHeight;
        var scrollTop = $(window).scrollTop();


        switch (_dataPage) {
            case 'home':
                if (scrollTop + wHeight >= dHeight - _headerH) {
                    $header.addClass('h-white');
                } else {
                    if ($header.hasClass('check-footer')) {
                        $header.removeClass('h-white');
                    }
                }
                break;
            case 'about-approach':
                if (scrollTop + wHeight >= dHeight - _headerH) {
                    $header.removeClass('h-white');
                } else {
                    $header.addClass('h-white');
                }
                break;
            case 'journal-inner':
                if (scrollTop >= (wHeight * 0.80) - _headerH) {
                    if (scrollTop + wHeight >= dHeight - _headerH) {
                        $header.addClass('h-white');
                    } else {
                        $header.removeClass('h-white');
                    }
                } else {
                    $header.addClass('h-white');
                }

                break;
            case 'contact':
                $header.addClass('h-white');

                break;
            case 'services':
                var _scTop = $('.sc-clients').offset().top;

                if (scrollTop >= _scTop - _headerH) {
                    if (scrollTop + wHeight >= dHeight - _headerH) {
                        $header.addClass('h-white');
                        console.log('footer');
                    } else {
                        $header.removeClass('h-white');
                        console.log('crientes');
                    }

                } else {
                    if (!$header.hasClass('check-header')) {
                        $header.addClass('h-white');
                    }
                }
                break;
            case 'service':

                var _scTop = $('.sc-testimonials').offset().top;

                if (scrollTop >= _scTop - _headerH) {
                    if (scrollTop + wHeight >= dHeight - _headerH) {
                        $header.addClass('h-white');
                    } else {
                        $header.removeClass('h-white');
                    }

                } else {
                    $header.addClass('h-white');
                }
                break;
            default:
                if (scrollTop + wHeight >= dHeight - _headerH) {
                    $header.addClass('h-white');
                } else {
                    $header.removeClass('h-white');
                }
        }
    }

    function showMenu() {
        $header.addClass('show-menu');
        $menu.addClass('show-menu');
        $hmbrg.addClass('active');
    }

    function hideMenu() {
        $header.removeClass('show-menu');
        $menu.removeClass('show-menu');
        $hmbrg.removeClass('active');
    }
}
header();
// & Header 

// ~ Loader 
const loader = () => {
    var $loader = $('.loader');

    function initMotion() {
        $loader.css({
            'display': 'flex'
        });
        console.log('initMotion')
    }

    function inMotion() {
        $loader.addClass('motion-in');
    }


    function progressMotion(_percent) {

        var _rounded = Math.round(_percent / 20);

        if (_rounded < 1) {
            $loader.find('.background').addClass('b-blue');
        } else if (_rounded >= 1 && _rounded < 2) {
            $loader.find('.background').addClass('b-green');
        } else if (_rounded >= 2 && _rounded < 3) {
            $loader.find('.background').addClass('b-purple');
        } else if (_rounded >= 3 && _rounded < 4) {
            $loader.find('.background').addClass('b-orange');
        } else if (_rounded > 4) {
            $loader.find('.background').addClass('b-yellow');
        }
    }

    function outMotion(_steps) {

        if (_steps == '1') {
            $loader.addClass('motion-out-1');
        } else {
            $loader.addClass('motion-out-2');
        }
    }

    function hide() {
        $loader.css({
            'display': 'none'
        });
        $loader.removeClass('motion-out-1').removeClass('motion-out-2').removeClass('motion-in');
        $loader.find('.background').removeClass('b-blue').removeClass('b-green').removeClass('b-purple').removeClass('b-orange').removeClass('b-yellow');
    }

    return {
        initMotion,
        inMotion,
        progressMotion,
        outMotion,
        hide
    }
}
loader().initMotion();
loader().inMotion();
loader().progressMotion();
loader().outMotion();
loader().hide();
// console.log(loader().loader)
// ~ Loader 

// TODO Mousemove 
const mouseMove = () => {
    var isMobile = (function(a) {
        return /(android|bb\d+|meego).+mobile|avantgo|bada\/|blackberry|blazer|compal|elaine|fennec|hiptop|iemobile|ip(hone|od)|iris|kindle|lge |maemo|midp|mmp|mobile.+firefox|netfront|opera m(ob|in)i|palm( os)?|phone|p(ixi|re)\/|plucker|pocket|psp|series(4|6)0|symbian|treo|up\.(browser|link)|vodafone|wap|windows ce|xda|xiino/i.test(a) || /1207|6310|6590|3gso|4thp|50[1-6]i|770s|802s|a wa|abac|ac(er|oo|s\-)|ai(ko|rn)|al(av|ca|co)|amoi|an(ex|ny|yw)|aptu|ar(ch|go)|as(te|us)|attw|au(di|\-m|r |s )|avan|be(ck|ll|nq)|bi(lb|rd)|bl(ac|az)|br(e|v)w|bumb|bw\-(n|u)|c55\/|capi|ccwa|cdm\-|cell|chtm|cldc|cmd\-|co(mp|nd)|craw|da(it|ll|ng)|dbte|dc\-s|devi|dica|dmob|do(c|p)o|ds(12|\-d)|el(49|ai)|em(l2|ul)|er(ic|k0)|esl8|ez([4-7]0|os|wa|ze)|fetc|fly(\-|_)|g1 u|g560|gene|gf\-5|g\-mo|go(\.w|od)|gr(ad|un)|haie|hcit|hd\-(m|p|t)|hei\-|hi(pt|ta)|hp( i|ip)|hs\-c|ht(c(\-| |_|a|g|p|s|t)|tp)|hu(aw|tc)|i\-(20|go|ma)|i230|iac( |\-|\/)|ibro|idea|ig01|ikom|im1k|inno|ipaq|iris|ja(t|v)a|jbro|jemu|jigs|kddi|keji|kgt( |\/)|klon|kpt |kwc\-|kyo(c|k)|le(no|xi)|lg( g|\/(k|l|u)|50|54|\-[a-w])|libw|lynx|m1\-w|m3ga|m50\/|ma(te|ui|xo)|mc(01|21|ca)|m\-cr|me(rc|ri)|mi(o8|oa|ts)|mmef|mo(01|02|bi|de|do|t(\-| |o|v)|zz)|mt(50|p1|v )|mwbp|mywa|n10[0-2]|n20[2-3]|n30(0|2)|n50(0|2|5)|n7(0(0|1)|10)|ne((c|m)\-|on|tf|wf|wg|wt)|nok(6|i)|nzph|o2im|op(ti|wv)|oran|owg1|p800|pan(a|d|t)|pdxg|pg(13|\-([1-8]|c))|phil|pire|pl(ay|uc)|pn\-2|po(ck|rt|se)|prox|psio|pt\-g|qa\-a|qc(07|12|21|32|60|\-[2-7]|i\-)|qtek|r380|r600|raks|rim9|ro(ve|zo)|s55\/|sa(ge|ma|mm|ms|ny|va)|sc(01|h\-|oo|p\-)|sdk\/|se(c(\-|0|1)|47|mc|nd|ri)|sgh\-|shar|sie(\-|m)|sk\-0|sl(45|id)|sm(al|ar|b3|it|t5)|so(ft|ny)|sp(01|h\-|v\-|v )|sy(01|mb)|t2(18|50)|t6(00|10|18)|ta(gt|lk)|tcl\-|tdg\-|tel(i|m)|tim\-|t\-mo|to(pl|sh)|ts(70|m\-|m3|m5)|tx\-9|up(\.b|g1|si)|utst|v400|v750|veri|vi(rg|te)|vk(40|5[0-3]|\-v)|vm40|voda|vulc|vx(52|53|60|61|70|80|81|83|85|98)|w3c(\-| )|webc|whit|wi(g |nc|nw)|wmlb|wonu|x700|yas\-|your|zeto|zte\-/i.test(a.substr(0, 4));
    })(navigator.userAgent || navigator.vendor || window.opera);

    function init(_element) {

        var $el = _element;

        if (!isMobile) {
            $el.on('mouseenter', function() {
                $el.on('mousemove', function(e) {
                    mouseMove(this, e);
                });
            });

            $el.on('mouseleave', function() {
                mouseOut(this);
            });
        }
    }

    function mouseMove(_this, _e) {

        var $this = _this;
        var x = _e.pageX - $($this).offset().left,
            y = _e.pageY - $($this).offset().top;

        var px = x / $($this).width(),
            py = y / $($this).height();

        var xx = -20 + (30 * px),
            yy = 20 - (30 * py);

        TweenMax.killTweensOf($($this));
        TweenMax.to($($this), 1, {
            rotationY: xx,
            rotationX: yy,
            rotationZ: 0,
            transformPerspective: 1000,
            ease: Quad.easeOut
        });
    }

    function mouseOut(_this) {

        var $this = _this;

        $($this).off('mousemove');

        TweenMax.killTweensOf($($this));
        TweenMax.to($($this), .5, {
            rotationY: 0,
            rotationX: 0,
            rotationZ: 0,
            transformPerspective: 1000,
            ease: Quad.easeOut
        });
    }

    return {
        init,
        mouseMove,
        mouseOut,
        isMobile
    }
    // console.log('mouse move')
}
// mouseMove();
console.log('mouse move',mouseMove())
// TODO Mousemove 

// ! Services Items
const serviceItems = () => {
    var $svcsItem = $('.services-items');

    if ($svcsItem.length) {
        $svcsItem.slick({
            infinite: false,
            slidesToShow: 1,
            slidesToScroll: 1,
            arrows: false,
            variableWidth: true,
            responsive: [{
                breakpoint: 768,
                settings: {
                    arrows: false,
                }
            }]
        });

        $svcsItem.find('.t-prev').on('click', function() {
            $svcsItem.find('.slides').slick('slickPrev');
        });

        $svcsItem.find('.t-next').on('click', function() {
            $svcsItem.find('.slides').slick('slickNext');
        });

    }

    // console.log('service items')
}
serviceItems();
// ! Services Items

// ? Testimonial
const testimonials = () => {
    var $testimonial = $('.testimonial:not(.no-slick)');
    if ($testimonial.length) {

        $testimonial.find('.slides').slick({
            infinite: true,
            lazyLoad: 'ondemand',
            slidesToShow: 3,
            slidesToScroll: 1,
            arrows: false,
            dots: true,
            centerMode: true,
            variableWidth: true,
            responsive: [{
                breakpoint: 768,
                settings: {
                    arrows: false,
                    centerMode: true,
                    slidesToShow: 1
                }
            }]
        });

        $testimonial.find('.t-prev').on('click', function() {
            $testimonial.find('.slides').slick('slickPrev');
        });

        $testimonial.find('.t-next').on('click', function() {
            $testimonial.find('.slides').slick('slickNext');
        });

    }
}
testimonials();
// ? Testimonial

// ^ Title
const title = () => {
    function init(_target) {
        $(_target).each(function() {
    
            var _html = $(this).html().replace('<strong>', ' -- ').replace('</strong>', ' -- ').replace('<br>', ' ').split(' ');
            var _bold = '';
            var _count = 0
    
            $(this).html(' ');
    
            for (var i = 0; i < _html.length; i++) {
    
                if (_html[i] == '--') {
                    if (_count == 0) {
                        _bold = 'bold';
                    } else {
                        _bold = '';
                    }
    
                    _count++;
                }
    
                if (_html[i] == '|') {
                    $(this).append('<div class="words w-block"><span></span></div>');
                } else if (_html[i] != '' && _html[i] != ' ' && _html[i] != '|' && _html[i] != '--') {
                    $(this).append('<div class="words ' + _bold + '"><span>' + _html[i] + '</span></div>');
                }
            }
        });
    }

    function motionOut(_target) {
    
        $(_target).find('.words').each(function(i, e) {
            $(e).removeClass('motion-in');
        });
    }
    
    function motionIn(_target) {
    
        $(_target).find('.words').each(function(i, e) {
            $(e).addClass('motion-in');
        });
    }
    
    return {
        init,
        motionIn,
        motionOut
    }
}
title().init();
title().motionIn();
title().motionOut();
console.log(title(), 'title')
// ^ Title

// * load
const load = () => {
    function init(opts) {

        var before = opts.before || function() {};
        var progress = opts.progress || function() {};
        var complete = opts.complete || function() {};
        var testDelay = opts.testDelay || 0;
        var $elem = $(opts.elem || 'body');
    
        var urls = [];
        var queue = [];
        var isLoaded;
    
        $elem.find('*').add($elem).each(function() {
            var $elem = $(this);
            var bgImg = $elem.css('background-image');
            var bgUrl = (/(^url\([\'\"]?)([^\"\']*)([\'\"]?\))/.exec(bgImg) || [])[2];
            var url;
            if ($elem.is('img')) url = $elem.attr('src');
            else if (bgUrl) url = bgUrl;
            if (url && urls.indexOf(url) < 0) {
                urls.push(url);
                queue.push({
                    src: url,
                    progress: 0
                });
            }
        });
    
        if (!urls.length) return complete(urls);
    
        before();
    
        function checkProgress() {
    
            var loaded = 0;
            var total = queue.length;
    
            queue.forEach(function(item) {
                loaded = loaded + item.progress;
            });
    
            var pcent = loaded * 100 / total;
            progress(pcent);
    
            if (pcent >= 100 && !isLoaded) {
                isLoaded = true;
                complete(urls);
            }
    
        }
    
        queue.forEach(function(item, i) {
            setTimeout(function() {
    
                var req = new XMLHttpRequest();
    
                req.onprogress = function(e) {
                    item.progress = e.loaded / e.total;
                    checkProgress();
                };
    
                req.onloadend = req.ontimeout = req.onerror = req.onabort = function(e) {
                    item.progress = 1;
                    checkProgress();
                };
    
                req.open('GET', item.src, true);
                req.send(null);
    
            }, testDelay * (i + 1));
        });
    
    }

    return {
        init,
    }
}
load();
// console.log(load())
// * Load

// ^ Page Load
const pageLoad = () => {
    let Loader = loader();
    let Load = load();
    console.log(Loader, 'loader')
    console.log(Load, 'load')

    var $menu = $('.menu');
    var $header = $('.header');
    var $cont = $('.main-content');
    var $main = $('main');

    var _loadDelay;
    var _dataPage = $main.data('page');

    console.log('new page:', _dataPage)

    if (_dataPage == 'home') {
        _loadDelay = 250;
    } else {
        _loadDelay = 0
    }

    function init() {

        console.log('page loading in page-load')
        // let Loader = loader;

        setTimeout(function() {

            Loader.initMotion();

            Load.init({
                elem: $('body'),
                testDelay: _loadDelay,
                progress: function(pcent) {
                    if (_dataPage == 'home') {
                        var _pcent = parseInt(pcent);
                        Loader.progressMotion(_pcent);
                    }
                },
                complete: function() {

                    if (_dataPage == 'contact' || _dataPage == 'about-approach' || _dataPage == 'about-person' || _dataPage == 'legals') {
                        console.log('checking dataPage', _dataPage)
                        setTimeout(function() {
                            Loader.progressMotion(0);
                            setTimeout(function() {
                                Loader.progressMotion(20);
                                setTimeout(function() {
                                    Loader.progressMotion(20);
                                    setTimeout(function() {
                                        Loader.progressMotion(20);
                                        setTimeout(function() {
                                            Loader.progressMotion(20);
                                            setTimeout(function() {
                                                Loader.progressMotion(20);
                                            }, 0);
                                        }, 0);
                                    }, 0);
                                }, 0);
                            }, 0);
                        }, 0);

                        setTimeout(function() {
                            setFirstLoader();
                        }, 0);

                    } else {
                        setFirstLoader();
                    }
                },
            });

        }, 0);
    }

    function setFirstLoader() {
        setTimeout(function() {
            setTimeout(function() {
                Loader.hide();
                $menu.trigger('enter').addClass('motion-in');
                $header.addClass('motion-in');
                $cont.find('> *').trigger('enter').addClass('motion-in').attr('data-url', location.href);
                activeMenus(_dataPage);
                console.log('inside setFirstLoader')
            }, 0);
        }, 0);
    }

    function activeMenus(page) {
        $menu.find('.item').removeClass('active');
        $menu.find('.item').filter('[data-target="' + page + '"]').addClass('active');

        console.log('data-page ' + page);
    }

    return {
        init,
        setFirstLoader,
        activeMenus
    }
}
pageLoad();
pageLoad().init();
pageLoad().setFirstLoader();
pageLoad().activeMenus();
console.log('pageload init', pageLoad())

// ^ Page Load

// & Scroll Mageic
const scrollMagic = () => {
    ScrollMagicPluginGsap(ScrollMagic, TweenMax, TimelineMax);
    gsap.registerPlugin(ScrollToPlugin);

    console.log('inside scrollmagic')
}
// scrollMagic();
// & Scroll Mageic

// ~ SVG
const svg = () => {
    $('.svg').each(startClass);

    function startClass() {

        var $icon = $(this);
        if ($icon.hasClass('svg-inline')) return true;

        var bg = $icon.css('background-image').replace('url(', '').replace(')', '').replace(/\"/g, '');

        if (bg && bg != 'none') $.get(bg, function(resp) {
            $icon.html($(resp).find('svg')).addClass('svg-inline').trigger('svg-ready');
        });
    }

    console.log('svg')
}
svg();
// ~ SVG

// TODO About Approach
let MouseMove = mouseMove()
const aboutApproch = () => {
    var $abtApproach = $('.approach');
    var $item = $abtApproach.find('.item');
    var $videos = $('.video-background');

    function init() {

        if ($(window).width() >= 860) {
            MouseMove.init($abtApproach.find('.item').find('.wrapper'));

            $item.on('mouseenter', onMouseEnter);
            $item.on('mouseleave', onMouseLeave);

            $('video')[0].play();
            $('video')[1].play();
            $('video')[2].play();
        }
    }

    function resize() {}

    function onMouseEnter() {
        console.log('inside onMouseEnter');
        var _target = $(this).data('video');

        $item.addClass('hide');
        $(this).removeClass('hide');

        TweenMax.killTweensOf($('.title.t-center'));
        TweenMax.killTweensOf($('.header'));
        TweenMax.killTweensOf($('.submenu'));

        new TimelineMax()
            .add([
                TweenMax.to($('.title.t-center'), 0.5, {
                    ease: Power3.easeOut,
                    opacity: 0
                }),
                TweenMax.to($('.header'), 0.1, {
                    ease: Power3.easeOut,
                    opacity: 0
                }, 0),
                TweenMax.to($('.submenu'), 0.2, {
                    ease: Expo.easeOut,
                    opacity: 0,
                }, 0)
            ]);

        $videos.find('.video').removeClass('active');
        $videos.find('[data-target="' + _target + '"]').addClass('active');
    }

    function onMouseLeave() {

        $item.removeClass('hide');
        $videos.find('.video').removeClass('active');

        TweenMax.killTweensOf($('.title.t-center'));
        TweenMax.killTweensOf($('.header'));
        TweenMax.killTweensOf($('.submenu'));

        new TimelineMax()
            .add([
                TweenMax.to($('.title.t-center'), 0.5, {
                    ease: Power3.easeOut,
                    opacity: 1
                }),
                TweenMax.to($('.header'), 0.1, {
                    ease: Power3.easeOut,
                    opacity: 1
                }, 0),
                TweenMax.to($('.submenu'), 0.2, {
                    ease: Expo.easeOut,
                    opacity: 1,
                }, 0)
            ]);

    }
    
    return {
        init,
        resize,
        onMouseEnter,
        onMouseLeave
    }
}
aboutApproch();
// TODO About Approach'

// ! About Team
const aboutTeam = () => {
    var tl = gsap.timeline()
    var $webdoor = $('.webdoor');
    var $peopleList = $('.people');
    var $peopleScramble = $('.people-scramble');
    var $itemScramble = $peopleScramble.find('.item');
    var $name = document.querySelector('.name');
    var $area = document.querySelector('.area');
    var _fxName = null;
    var _fxArea = null;
    var _name = null;
    var _area = null;
    var _arrPos = [
        [12.5 * 0, 12.5 * 1, 12.5 * 1, 12.5 * 0],
        [12.5 * 1, 12.5 * 2, 12.5 * 2, 12.5 * 1],
        [12.5 * 2, 12.5 * 3, 12.5 * 3, 12.5 * 2],
        [12.5 * 3, 12.5 * 4, 12.5 * 4, 12.5 * 3],
        [12.5 * 4, 12.5 * 5, 12.5 * 5, 12.5 * 4],
        [12.5 * 5, 12.5 * 6, 12.5 * 6, 12.5 * 5],
        [12.5 * 6, 12.5 * 7, 12.5 * 7, 12.5 * 6],
        [12.5 * 7, 12.5 * 8, 12.5 * 8, 12.5 * 7]
    ]

    function init() {

        if ($(window).width() >= 860) {
            _fxName = new TextScramble($name);
            _fxArea = new TextScramble($area);

            mouseMove().init($peopleList.find('.item').find('.i-wrapper'));

            $itemScramble.on('mouseenter', mouseEnter);
            $itemScramble.on('mouseleave', mouseLeave);

            $peopleScramble.on('mouseleave', function() {
                _fxName.setText('the people');
                _fxArea.setText('magic');
            });
        }
    }

    function resize() {}

    function mouseEnter() {
        var _target = $(this).data('target');
        var _name = $(this).data('name');
        var _area = $(this).data('area');

        $itemScramble.each(function(i, e) {

            var $figure = $peopleScramble.find('.wrapper').find('figure').eq(i);

            gsap.killTweensOf($figure);

            if (i == _target) {
                gsap.to($figure, 1, {
                    ease: Power3.easeOut,
                    "clip-path": "polygon(0% 0%, 100% 0%, 100% 100%, 0% 100%)"
                });
            } else {

                if ((i) < _target) {
                    gsap.to($figure, 1, {
                        ease: Power3.easeOut,
                        "clip-path": "polygon(0% 0%, 0% 0%, 0% 100%, 0% 100%)"
                    });
                } else {
                    gsap.to($figure, 1, {
                        ease: Power3.easeOut,
                        "clip-path": "polygon(100% 0%, 100% 0%, 100% 100%, 100% 100%)"
                    });
                }
            }
        });

        _fxName.setText(_name);
        _fxArea.setText(_area);
    }

    function mouseLeave() {
        var $this = $(this);

        $itemScramble.each(function(i, e) {
            var _target = $(this).data('target');
            var $figure = $this.closest('.wrapper').find('figure').eq(_target);

            TweenMax.killTweensOf($figure);
            TweenMax.to($figure, 3, {
                ease: Elastic.easeOut,
                "clip-path": "polygon(" + _arrPos[i][0] + "% 0%, " + _arrPos[i][1] + "% 0%, " + _arrPos[i][2] + "% 100%, " + _arrPos[i][3] + "% 100%)"
            });
        });
    }

    class TextScramble {
        constructor(el) {
            this.el = el
            this.chars = 'abcdefghijlkmnopqrstuvxz'
            this.update = this.update.bind(this)
        }

        setText(newText) {
            const oldText = this.el.innerText
            const length = Math.max(oldText.length, newText.length)
            const promise = new Promise((resolve) => this.resolve = resolve)
            this.queue = []
            for (let i = 0; i < length; i++) {
                const from = oldText[i] || ''
                const to = newText[i] || ''
                const start = Math.floor(Math.random() * 40)
                const end = start + Math.floor(Math.random() * 40)
                this.queue.push({
                    from,
                    to,
                    start,
                    end
                })
            }
            cancelAnimationFrame(this.frameRequest)
            this.frame = 0
            this.update()
            return promise
        }
        update() {
            let output = ''
            let complete = 0
            for (let i = 0, n = this.queue.length; i < n; i++) {
                let {
                    from,
                    to,
                    start,
                    end,
                    char
                } = this.queue[i]
                if (this.frame >= end) {
                    complete++
                    output += to
                } else if (this.frame >= start) {
                    if (!char || Math.random() < 0.28) {
                        char = this.randomChar()
                        this.queue[i].char = char
                    }
                    output += `<span class="dud">${char}</span>`
                } else {
                    output += from
                }
            }
            this.el.innerHTML = output
            if (complete === this.queue.length) {
                this.resolve()
            } else {
                this.frameRequest = requestAnimationFrame(this.update)
                this.frame++
            }
        }
        randomChar() {
            return this.chars[Math.floor(Math.random() * this.chars.length)]
        }
    }

    return {
        init,
        resize, 
        mouseEnter,
        mouseLeave
    }
}
aboutTeam().init();
aboutTeam().resize();
// aboutTeam().mouseEnter();
// aboutTeam().mouseLeave();
// ! About Team

// ? Homepage
const Title = title();
const Earth = earth();
const homePage = () => {
    var $pages       	= $('.pages');
    var $header         = $('.header');
    var $scrollDown     = $('.scrolldown');
    var $homeBullets    = $('.home-bullets');
    var _controller     = null;
    var _wHeight        = window.innerHeight;
    var _scrollPos      = 0;
    var _scrollValues   = [];

    function init(){
        $pages.find('.infos').height(window.innerHeight);
        $pages.find('.circle').css({'width':window.innerHeight/1.8, 'height':window.innerHeight/1.8});
        Title.init($pages.find('.title'));
        addScrollMagic();
        setScrollTo();
        Earth.init();
        resize();
    }

    function resize() {
        $pages.find('.infos').height(window.innerHeight);
        if($(window).width() > 500){
            $pages.find('.circle').css({'width':window.innerHeight/1.8, 'height':window.innerHeight/1.8});
        }
        earth().resize();
        _controller.update(true);
    }

    function addScrollMagic(){
        var $slides     = $pages.find('.hc-slides');
        var $slidesC    = $pages.find('.hc-clients');
        _scrollValues   = [];
        _controller = new ScrollMagic.Controller({
            globalSceneOptions: {
                triggerHook: 0,
                reverse: true,
            }
        });

        $slides.find('.infos').each(function(e){
            var _this       = this;
            var _index      = e;
            var _color      = $(this).data('color');
            var scene = new ScrollMagic.Scene({triggerElement: this, duration: "100%"})
                            .setPin(this)
                            .on("enter", function (e) {
                                $slides.find('.home-bckg').css({background:_color});
                                if( _index == 0){
                                    gsap.to($('.globe').find('.g-wrapper'), 0.4, { ease: Power3.easeOut, opacity: 1, y: '0%' },0);
                                }else{
                                    gsap.to($slides.find('.images').find('.img').eq(_index), 0.5, {ease: Power3.easeOut, opacity: 1},0);
                                }
                                $homeBullets.find('.button').removeClass('active');
                                $homeBullets.find('.button').eq(_index).addClass('active');
                                Title.motionIn($(_this));
                                $(_this).find('.paragraph').addClass('motion-in');
                                $(_this).find('.button').addClass('motion-in');
                                _scrollPos = _index;
                            })
                            .on("leave", function (e) {
                                if(e.scrollDirection == "FORWARD"){
                                    if( _index == 0){
                                        gsap.to($('.globe').find('.g-wrapper'), 0.4, { ease: Power3.easeOut, opacity: 0, y: '-10%' },0);
                                    }else{
                                        gsap.to($slides.find('.images').find('.img').eq(_index), 0.5, {ease: Power3.easeOut, opacity: 0},0);
                                    }
                                }

                                Title.motionOut($(_this));
                                $(_this).find('.paragraph').removeClass('motion-in');
                                $(_this).find('.button').removeClass('motion-in');
                            
                            })
                            .addTo(_controller);
            // var homeShape = new TimelineMax()
            // .add([
            //     gsap.to($pages.find('.shape-image').eq(_index), 2, {opacity: 1},0)
            // ]);

            var homeShape = new TimelineMax()
            .to($pages.find('.shape-image').eq(_index), 2, {opacity: 1},0)

            new ScrollMagic.Scene({triggerElement: this, duration:'100%', offset:-_wHeight}).setTween(homeShape).addTo(_controller);

            var homeImages = new TimelineMax()
            .add([
                gsap.fromTo($slides.find('.images').find('.img').eq(_index).find('img'), 2, {x: '-20%', opacity: 0},{x: '0%', opacity: 1},0)
            ]);
            
            new ScrollMagic.Scene({triggerElement: this, duration:'100%', offset:-_wHeight/2}).setTween(homeImages).addTo(_controller);

            _scrollValues.push(scene.scrollOffset()+(scene.duration()-10));
            
        });

        $slidesC.find('.infos').each(function(e){
            var _this       = this;
            var scene = new ScrollMagic.Scene({triggerElement: this, duration: "100%"})
                            .setPin(this)
                            .on("enter", function (e) {
                                
                                $header.removeClass('h-white').addClass('check-footer');
                                $scrollDown.removeClass('s-white').removeClass('hide');
                                $homeBullets.find('.button').removeClass('active');
                                $homeBullets.find('.button').eq(4).addClass('active');
                                $homeBullets.addClass('hb-dark');
                                Title.motionIn($(_this));

                                gsap.to($homeBullets, 1, {ease: Power3.easeOut, x: '0%'},0);

                                $(_this).find('.image').each(function(i,e){
                                    gsap.to($(e), 1, {ease: Power3.easeOut, opacity: 1},0);
                                });

                                _scrollPos = 4;
                            })
                            .on("leave", function (e) {
                                $homeBullets.removeClass('hb-dark');
                                
                                if(e.scrollDirection == "REVERSE"){
                                    $scrollDown.addClass('s-white').removeClass('hide');
                                    $header.addClass('h-white').removeClass('check-footer');
                                }

                                if(e.scrollDirection == "FORWARD"){
                                    gsap.to($homeBullets, 1, {ease: Power3.easeOut, x: '-200px'},0);
                                    $scrollDown.addClass('hide');
                                }

                                $(_this).find('.image').each(function(){
                                    gsap.to($(this), 1, {ease: Power3.easeOut, opacity: 0},0);
                                });

                                Title.motionOut($(_this));    
                            })
                            .addTo(_controller);
            
            _scrollValues.push(scene.scrollOffset()+(scene.duration()-10));

        });
    }

    function setScrollTo(){
        _controller.scrollTo(function (newScrollPos) {
            gsap.to(window, 2, {scrollTo: {y: newScrollPos , ease: Power3.easeOut}});
        });
        $homeBullets.find('.button').on('click',function(){
            var _target = parseInt($(this).data('target').split('slide-')[1]);
            _controller.scrollTo(_scrollValues[_target-1]);
        });
        $scrollDown.on('click',function(){
            if(_scrollPos == 4){
                _controller.scrollTo($(document).height());
            }else{
                _controller.scrollTo(_scrollValues[_scrollPos+1]);
            }
        });
    }
    return { init, resize, addScrollMagic, setScrollTo }
}
homePage().init();
// homePage().addScrollMagic();
// homePage().setScrollTo();
// ? Homepage

// ^ About Page
const about = () => {
    var $abtWorld = $('.about-world');
    var $people = $abtWorld.find('.item');
    var $highlights = $('.highlights');
    var _inneHeight = $abtWorld.find('.aw-people').height();
    var _inneWidth = $abtWorld.find('.aw-people').width();

    function init() {
        addPopPeople();
        if ($(window).width() >= 860) {
            MouseMove.init($highlights.find('img'));
        }
        console.log('inside about.js')
    }
    function resize() {}

    function addPopPeople() {
        $people.each(function(e, i) {
            var _this = this;
            setTimeout(function() {
                scramblePos(_this);
            }, e * 300);
        });
    }

    function getRandomInt(min, max) {
        return Math.floor(Math.random() * (max - min + 1)) + min;
    }

    function scramblePos(_this) {
        var _posY = getRandomInt(1, _inneHeight);
        var _posX = getRandomInt(1, _inneWidth);

        $(_this).css('transform', 'translate(' + _posX + 'px,' + _posY + 'px)');

        motionIn(_this);
    }

    function motionIn(_this) {
        var $item = $(_this);
        var $figure = $(_this).find('img');

        new TimelineMax({
                onComplete: function() {
                    TweenMax.killTweensOf($figure);
                    setTimeout(function() {
                        scramblePos($item);
                    }, 1000);
                }
            })
            .add([TweenMax.to($figure, 5, {
                scale: 1,
                ease: 'Elastic.easeOut'
            })])
            .add([TweenMax.to($figure, .5, {
                scale: 0,
                ease: 'Power3.easeOut'
            })], 20)
    }
    return {
        init,
        resize
    }
}
about().init();
about().resize();
// ^ About Page

// * Contact
const Form = form();
const contact = () => {
    var $contact = $('.contact');
    var $input = $contact.find('.input');
    var $scrollDown = $('.scrolldown');
    function init() {
        Form.init($input);
        $scrollDown.on('click', function() {
            $(this).addClass('hide');
            gsap.to(window, .5, {
                scrollTo: {
                    y: $(window).height(),
                    ease: Power3.easeOut
                }
            });
        });
    }

    function resize() {}

    return {
        init,
        resize
    }
}
contact().init();
contact().resize();
// * Contact

// & works
const work = () => {
    var $workList = $('.works-list');
    function init() {
        if ($(window).width() >= 860) {
            MouseMove.init($workList.find('.item').find('.wrapper'));
        }
    }

    function resize() {}

    return {
        init,
        resize
    }
}
work();

// ~ Service
const service = () => {
    var $pages = $('.pages');
    var $header = $('.header');
    var $scrollDown = $('.scrolldown');
    var $workList = $('.works-list');
    var _controller = null;
    var _scrollPos = 0;
    var _scrollValues = [];

    function init() {
        $pages.find('.sc-slides').find('.infos').height(window.innerHeight);
        Title.init($pages.find('.sc-slides').find('.title'));
        MouseMove.init($workList.find('.item').find('.wrapper'));
        addScrollMagic();
        setScrollTo();
        resize();
    }

    function resize() {
        $pages.find('.sc-slides').find('.infos').height(window.innerHeight);
        _controller.update(true);
    }

    function addScrollMagic() {
        var $slides = $pages.find('.sc-slides');
        _scrollValues = [];
        _controller = null;
        _controller = new ScrollMagic.Controller({
            globalSceneOptions: {
                triggerHook: 0,
                reverse: true,
            }
        });

        $slides.find('.infos').each(function(e) {
            var _this = this;
            var _index = e;
            var _color = $(this).data('color');
            var scene = new ScrollMagic.Scene({
                    triggerElement: this,
                    duration: "100%"
                })
                .setPin(this)
                .on("enter", function(e) {
                    $slides.find('.services-bckg').css({
                        background: _color
                    });
                    $header.addClass('h-white').addClass('check-header');
                    _scrollPos = _index;
                    if (_index == 0) {
                        Title.motionIn($(_this));
                        $(_this).find('.caption').addClass('motion-in');
                        $(_this).find('.submenu').addClass('motion-in');
                        setTimeout(function() {
                            $(_this).find('.paragraph').addClass('motion-in');
                            $(_this).find('.button').addClass('motion-in');
                            $(_this).find('.image-services').addClass('motion-in');
                        }, 1000);
                    }

                    if (_index == 1) {
                        $(_this).find('.caption').addClass('motion-in');
                        $(_this).find('.subtitle').addClass('motion-in');
                        $(_this).find('.paragraph').addClass('motion-in');
                        $(_this).find('.button').addClass('motion-in');
                    }

                    if (_index == 2) {
                        $scrollDown.removeClass('hide');
                        $(_this).find('.services-items').addClass('motion-in');
                    }
                })
                .on("leave", function(e) {
                    if (_index == 0) {
                        Title.motionOut($(_this));
                        $(_this).find('.submenu').removeClass('motion-in');
                        $(_this).find('.paragraph').removeClass('motion-in');
                        $(_this).find('.button').removeClass('motion-in');
                        $(_this).find('.caption').removeClass('motion-in');
                        $(_this).find('.image-services').removeClass('motion-in');
                    }

                    if (_index == 1) {
                        $(_this).find('.caption').removeClass('motion-in');
                        $(_this).find('.subtitle').removeClass('motion-in');
                        $(_this).find('.paragraph').removeClass('motion-in');
                        $(_this).find('.button').removeClass('motion-in');
                    }

                    if (_index == 2) {
                        if (e.scrollDirection == "FORWARD") {
                            $scrollDown.addClass('hide');
                        }
                        $(_this).find('.services-items').removeClass('motion-in');
                    }

                })
                .addTo(_controller);

                _scrollValues.push(scene.scrollOffset() + (scene.duration() - 10));

        });
    }

    function setScrollTo() {
        _controller.scrollTo(function(newScrollPos) {
            gsap.to(window, 1.5, {
                scrollTo: {
                    y: newScrollPos,
                    ease: Power3.easeOut
                }
            });
        });

        $scrollDown.on('click', function() {
            if (_scrollPos == 2) {
                _controller.scrollTo(_scrollValues[2] + $(window).height() + 10);
            } else {
                _controller.scrollTo(_scrollValues[_scrollPos + 1]);
            }
        });
    }

    return {
        init,
        resize,
        addScrollMagic,
        setScrollTo
    }
}
service();
// ~ Service

// ? Services
const services = () => {
    var $pages = $('.pages');
    var $header = $('.header');
    var $scrollDown = $('.scrolldown');
    var $svcsBullets = $('.services-bullets');
    var $svcsIcons = $('.services-icons');
    var $highlights = $('.highlights');
    var _controller = null;
    var _wHeight = window.innerHeight;
    var _scrollPos = 0;
    var _scrollValues = [];

    function init() {
        $pages.find('.infos').height(window.innerHeight);
        Title.init($pages.find('.sc-slides').find('.title'));
        MouseMove.init($highlights.find('img'));
        addScrollMagic();
        setScrollTo();
        resize();
    }

    function resize() {
        $pages.find('.infos').height(window.innerHeight);
        _controller.update(true);
    }

    function addScrollMagic() {
        var $slides = $pages.find('.sc-slides');
        _scrollValues = [];
        _controller = null;
        _controller = new ScrollMagic.Controller({
            globalSceneOptions: {
                triggerHook: 0,
                reverse: true,
            }
        });

        $slides.find('.infos').each(function(e) {
            var _this = this;
            var _index = e;
            var _color = $(this).data('color');
            var scene = new ScrollMagic.Scene({
                    triggerElement: this,
                    duration: "100%"
                })
                .setPin(this)
                .on("enter", function(e) {

                    $slides.find('.services-bckg').css({
                        background: _color
                    });

                    $svcsBullets.find('.button').removeClass('active');
                    $svcsBullets.find('.button').eq(_index).addClass('active');
                    $header.addClass('check-header');
                    gsap.to($svcsBullets, 1, {
                        ease: Power3.easeOut,
                        x: '0%'
                    }, 0);

                    _scrollPos = _index;

                    if (_index == 0) {
                        $header.removeClass('h-white')
                        $scrollDown.removeClass('s-white');
                        $svcsBullets.addClass('sb-dark');

                        Title.motionIn($(_this));
                        $(_this).find('.submenu').addClass('motion-in');
                        setTimeout(function() {
                            $(_this).find('.ico').addClass('motion-in');
                            $(_this).find('span').addClass('motion-in');
                        }, 1000);

                    }

                    if (_index >= 1) {
                        $header.addClass('h-white');
                        $scrollDown.addClass('s-white').removeClass('hide');
                        $svcsBullets.removeClass('sb-dark');

                        $(_this).find('.type').addClass('motion-in');
                        $(_this).find('.paragraph').addClass('motion-in');
                        $(_this).find('.button').addClass('motion-in');
                        $(_this).find('.ico').addClass('motion-in');
                        $(_this).find('.list').addClass('motion-in');
                    }

                })
                .on("leave", function(e) {

                    if (_index == 3) {

                        if (e.scrollDirection == "REVERSE") {
                            $scrollDown.removeClass('hide');
                        }

                        if (e.scrollDirection == "FORWARD") {
                            gsap.to($svcsBullets, 1, {
                                ease: Power3.easeOut,
                                x: '-200px'
                            }, 0);
                            $scrollDown.addClass('hide');
                            $header.addClass('h-white').removeClass('check-header');
                        }
                    }

                    if (_index == 0) {
                        Title.motionOut($(_this));
                        $(_this).find('.ico').removeClass('motion-in');
                        $(_this).find('.submenu').removeClass('motion-in');
                        $(_this).find('span').removeClass('motion-in');
                    }

                    if (_index >= 1) {
                        $(_this).find('.type').removeClass('motion-in');
                        $(_this).find('.paragraph').removeClass('motion-in');
                        $(_this).find('.button').removeClass('motion-in');
                        $(_this).find('.ico').removeClass('motion-in');
                        $(_this).find('.list').removeClass('motion-in');
                    }

                })
                .addTo(_controller);

            _scrollValues.push(scene.scrollOffset() + (scene.duration() - 10));
        });
    }

    function setScrollTo() {
        _controller.scrollTo(function(newScrollPos) {
            gsap.to(window, 1.5, {
                scrollTo: {
                    y: newScrollPos,
                    ease: Power3.easeOut
                }
            });
        });

        $svcsBullets.find('.button').on('click', function() {
            var _target = parseInt($(this).data('target').split('slide-')[1]);
            _controller.scrollTo(_scrollValues[_target - 1]);
        });

        $svcsIcons.find('.item').on('click', function() {
            var _target = parseInt($(this).data('target').split('slide-')[1]);
            _controller.scrollTo(_scrollValues[_target - 1]);
        });

        $scrollDown.on('click', function() {
            if (_scrollPos == 3) {
                _controller.scrollTo(_scrollValues[3] + $(window).height() + 10);
            } else {
                _controller.scrollTo(_scrollValues[_scrollPos + 1]);
            }

        });
    }

    return {
        init,
        resize
    }
}
services().init();
// ? Services

// ! Initialization of pages 
const pageInits = () => {
    const _pages = $('main').data('page');

    // PageLoad.init();
    // console.log('pageLoad', pageLoad())
    // pageLoad.init

    $(function() {
        'use strict';

        // console.log('pageLoad', pageLoad())
        console.log('INIT');

        switch (_pages) {
            case 'home':
                homePage();
                break;
            case 'about':
                console.log('about');
                // About.init();
                about();
                break;
            case 'about-approach':
                // Approach.init();
                aboutApproch();
                break;
            case 'about-team':
                // Team.init();
                aboutTeam();
                break;
            case 'works':
                // Works.init();
                work();
                break;
            case 'contact':
                // Contact.init();
                contact();
                break;
            case 'services':
                // Services.init();
                services();
                break;
            case 'service':
                // Service.init();
                service();
                break;
        }

        $(window).on('resize', function() {
            switch (_pages) {
                case 'home':
                    homePage().resize();
                    break;
                case 'services':
                    services().resize();
                    break;
                case 'service':
                    service().resize();
                    break;
            }

            console.log('resize');
        });

    });
}
pageInits();
// ! Initialization of Pages