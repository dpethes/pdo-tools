unit tabs;
{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Math, glist,
    pdo_common,
    utils, part_outliner; //for TEdgeVertex

const
  FLAPTYPE_QUAD = 0;
  FLAPTYPE_TRI = 1;

type
  TFlap = record
    ftype: byte;
    vertex_count: byte;
    vertices: array[0..3] of TEdgeVertex;
  end;

  TFlapList = specialize TList<TFlap>;

  PPdoEdge = ^TPdoEdge;

procedure GetLineTypes(var pdo_struct: TPdoStructure);
procedure SetLines(var pdo_struct: TPdoStructure);
function CreateTabCoords(var line: TPdoLineExtra; const vertex1, vertex2: TPdoFace2DVertex): TFlap;

//clip tabs against part's outline
procedure ClipTabs(tabs: TFlapList; const edges: TEdgeList);

implementation

type
  TLine = record
    x1, y1,
    x2, y2: single;
  end;

  TIntersectPoint = record
    x, y: single;
    has_value: boolean;
  end;

//find 2d vertex index by given 3d vertex index
function FindVertexById(const idx: integer; const vertices: array of TPdoFace2DVertex): integer;
var
  k: integer;
  vertex_count: integer;
begin
  result := 0;
  vertex_count := Length(vertices);
  for k := 0 to vertex_count - 1 do
  begin
      if vertices[k].id_vertex = idx then
      begin
          {need the vector of line after the joined edge}
          result := k + 1;
          if result = vertex_count then //=> wrap around
              result := 0;
          exit;
      end;
  end;
end;

{
  GetLineTypesForEdge

  Go through edges and set the edge line type based on angle between faces
}
procedure GetLineTypesForEdge(var obj: TPdoObject; edge: PPdoEdge; const hiding_angle: single);
const
  LineType: array[TValueSign] of byte = (1, 0, 2);
var
  face1, face2: TPdoFace;
  vert3d_idx1, vert3d_idx2: integer;
  vertid, vertid2: integer;
  vectx, vecty, vectz: double;
  dp, dp2, angle: double;
  angle_sign: TValueSign;

begin
  edge^.linetype := 0;  //cut edge as default
  if edge^.face2_index = -1 then
      exit;

  {get vertex number from face array}
  face1 := obj.faces[edge^.face1_index];
  face2 := obj.faces[edge^.face2_index];
  vertid := FindVertexById(edge^.vertex1index, face1.vertices);
  vertid2 := vertid + 1;
  if vertid2 >= Length(face1.vertices) then
      vertid2 := 0;

  {get vector of line pointing away from fold line}
  vert3d_idx1 := face1.vertices[vertid].id_vertex;
  vert3d_idx2 := face1.vertices[vertid2].id_vertex;
  vectx := obj.vertices[vert3d_idx2].x - obj.vertices[vert3d_idx1].x;
  vecty := obj.vertices[vert3d_idx2].y - obj.vertices[vert3d_idx1].y;
  vectz := obj.vertices[vert3d_idx2].z - obj.vertices[vert3d_idx1].z;

  {calculate the sign of the angle}
  {which is dot product of line pointing away in face1 and face2 normal}
  dp2 := vectx * face2.Nx + vecty * face2.Ny + vectz * face2.Nz;
  angle_sign := sign(dp2);
  edge^.linetype := LineType[angle_sign];

  {get angle here}
  {angle is dot product of both normals}
  dp := face1.Nx * face2.Nx + face1.Ny * face2.Ny + face1.Nz * face2.Nz;
  dp := clip3(-1, dp, 1);
  angle := radtodeg( arccos(dp) );  // * sign(dp2)?

  //if the faces angle is below treshold, set to cut line, which doesn't get exported (=hide almost flat lines)
  if abs(angle) < hiding_angle then
      edge^.linetype := 0;
end;

procedure GetLineTypes(var pdo_struct: TPdoStructure);
var
  angle: single;
  i, k: integer;
begin
  angle := max (0.1, 180 - pdo_struct.settings.fold_lines_hiding_angle);
  for i := 0 to Length(pdo_struct.objects) - 1 do
      for k := 0 to Length(pdo_struct.objects[i].edges) - 1 do
          GetLineTypesForEdge(pdo_struct.objects[i], @pdo_struct.objects[i].edges[k], angle);
end;


{
  SetupLineParams

  Sets vert2didx1, vert2didx2 and isflap parameters of Line, based on vertex it points to.
}
procedure SetupLineParams(var line_extra: TPdoLineExtra; var line: TPdoLine; const vertices: array of TPdoFace2DVertex; const vertex_index: integer);
var
  i, vertex_count: integer;
  vertex: TPdoFace2DVertex;
begin
  vertex_count := Length(vertices);
  for i := 0 to vertex_count - 1 do
  begin
      vertex := vertices[i];
      if vertex.id_vertex = vertex_index then
      begin
          line_extra.vert2didx1 := i;
          line_extra.vert2didx2 := i + 1;
          if i = vertex_count - 1 then
              line_extra.vert2didx2 := 0;

          //test for flap
          if (vertex.flap = 1) and (line.is_connecting_faces = False) then
              line_extra.isflap := True;

          break;
      end;
  end;
end;


procedure SetLines(var pdo_struct: TPdoStructure);
var
  i, j, k: integer;
  face_index, vertex_index: integer;
  lines: array of TPdoLine;
  lines_extra: array of TPdoLineExtra;
  obj: TPdoObject;
  edge: TPdoEdge;
begin
  for i := 0 to Length(pdo_struct.parts) - 1 do
  begin
      lines := pdo_struct.parts[i].lines;
      obj := pdo_struct.objects[ pdo_struct.parts[i].object_index ];

      SetLength(pdo_struct.parts[i].lines_extra, Length(lines));
      lines_extra := pdo_struct.parts[i].lines_extra;

      for j := 0 to Length(lines) - 1 do
      begin
          lines_extra[j].isflap := False;
          {which face}
          face_index := lines[j].face_index;
          {which vertex}
          {is stupid because face vertex would be better}
          vertex_index := lines[j].vertex_index;

          {set faces vertex idx for first line }
          lines_extra[j].vert2didx1 := 0;
          lines_extra[j].vert2didx2 := 0;
          lines_extra[j].Ltype := 0;

          for k := 0 to Length(obj.edges) - 1 do
          begin
              edge := obj.edges[k];

              {if face and vertex index matches}
              if (edge.face1_index = face_index) and (edge.vertex1index = vertex_index) then
              begin
                  SetupLineParams(lines_extra[j], lines[j], obj.faces[face_index].vertices, vertex_index);
                  lines_extra[j].Ltype := edge.linetype;

                  {added flaps line type}
                  if (edge.face2_index = -1) and (lines_extra[j].isflap = True) then
                      lines_extra[j].Ltype := lines[j].type_;

                  {no flap line type}
                  if lines_extra[j].isflap = False then
                  begin
                      lines_extra[j].Ltype := edge.linetype;
                      if edge.no_connected_face = 1 then
                          lines_extra[j].Ltype := 0;
                      if lines[j].is_connecting_faces = False then
                          lines_extra[j].Ltype := 0;
                  end;
              end;


              {do same for connected faces if they exist}
              if edge.face2_index <> -1 then
              begin
                  if (edge.face2_index = face_index) and (edge.vertex2index = vertex_index) then
                  begin
                      SetupLineParams(lines_extra[j], lines[j], obj.faces[face_index].vertices, vertex_index);
                      lines_extra[j].Ltype := edge.linetype;

                      {no flap line type}
                      if lines_extra[j].isflap = False then
                      begin
                          lines_extra[j].Ltype := edge.linetype;
                          if edge.no_connected_face = 1 then
                              lines_extra[j].Ltype := 0;
                          if lines[j].is_connecting_faces = False then
                              lines_extra[j].Ltype := 0;
                      end;
                  end;
              end;
          end;  {of k loop}

      end; {of j loop}

  end;  {of i loop}
end;


procedure CalcQuadFlapVertices(var line: TPdoLineExtra; const vy2, vx2, vy1, vx1: double; const vertex1: TPdoFace2DVertex);
var
  vx3, vy3: double;
  radius: double;
  alpha, cos_alpha, angle: double;
  xnew, ynew: double;
begin
  {1st tab vertex}
  {get angle to second vertex    (2nd vertex is 2nd vertex - first vertex) }

  {move to origin}
  vx3 := vx2 - vx1;
  vy3 := vy2 - vy1;

  {calculate angle bewteen x axis and 2nd certex}
  {division by 0 check and float errors check needed?????}
  cos_alpha := vx3 / Sqrt(vx3 * vx3 + vy3 * vy3);
  alpha := arccos(cos_alpha) * (180 / PI);


  //DP: how can this be NaN?
  if IsNan(vertex1.flap_a_angle) then
      exit;

  {temp vector to rotate about origin}
  radius := vertex1.flap_height / sin(vertex1.flap_a_angle);

  {get total angle needed}
  angle := alpha + (180 - (vertex1.flap_a_angle * (180 / PI)));
  if sign(vy3) = -1 then
      angle := -(180 + alpha) - ((vertex1.flap_a_angle * (180 / PI)));

  {rotate temp vector to get new vertex position}
  xnew := radius * cos(degtorad(angle));
  ynew := radius * sin(degtorad(angle));

  {move back and store vertex}
  line.flapvert1x := xnew + vx2;
  line.flapvert1y := ynew + vy2;

  {2nd tab vertex}

  {switch vertices around}
  vx3 := vx1 - vx2;
  vy3 := vy1 - vy2;
  alpha := 180 + alpha;
  radius := vertex1.flap_height / sin(vertex1.flap_b_angle);
  angle := alpha - (180 - (vertex1.flap_b_angle * (180 / PI)));
  if sign(vy3) = 1 then
      angle := -(180 + alpha) + ((vertex1.flap_b_angle * (180 / PI)));

  xnew := radius * cos(degtorad(angle));
  ynew := radius * sin(degtorad(angle));
  line.flapvert2x := xnew + vx1;
  line.flapvert2y := ynew + vy1;
end;


procedure CalcTriFlapVertex(var line: TPdoLineExtra; const vy2, vx2, vy1, vx1: double; const vertex1: TPdoFace2DVertex);
var
  vx3, vy3: double;
  radius: double;
  alpha, cos_alpha, angle: double;
  xnew, ynew: double;
begin
  {need to calculate new position for triangle flaps}
  {half way up edge at 90 degrees}
  vx3 := vx2 - vx1;
  vy3 := vy2 - vy1;
  cos_alpha := vx3 / Sqrt(vx3 * vx3 + vy3 * vy3);
  alpha := arccos(cos_alpha) * (180 / PI);
  radius := vertex1.flap_height;

  angle := alpha + (180 - (90));
  if sign(vy3) = -1 then
      angle := -(180 + alpha) - (90);

  xnew := radius * cos(degtorad(angle));
  ynew := radius * sin(degtorad(angle));
  line.flapvert1x := xnew + vx3 / 2 + vx1;
  line.flapvert1y := ynew + vy3 / 2 + vy1;
end;



function CreateTabCoords(var line: TPdoLineExtra; const vertex1, vertex2: TPdoFace2DVertex): TFlap;
const
 TAB_DELTA = 0.1;
var
 vx1, vy1: double;
 vx2, vy2: double;
 vx3, vy3: double;
 vx4, vy4: double;
 s1x, s1y, s2x, s2y: double;
 flap: TFlap;
begin
 vx1 := vertex1.x;
 vy1 := vertex1.y;
 vx2 := vertex2.x;
 vy2 := vertex2.y;

 CalcQuadFlapVertices(line, vy2, vx2, vy1, vx1, vertex1);

 {now test for intersection}
 vx3 := line.flapvert1x;
 vy3 := line.flapvert1y;
 vx4 := line.flapvert2x;
 vy4 := line.flapvert2y;
 s1x := vx2 - vx1;
 s1y := vy2 - vy1;
 s2x := vx3 - vx4;
 s2y := vy3 - vy4;

 {account for floating point errors effecting the sign}
 if abs(s1x) < TAB_DELTA then s1x := 0;
 if abs(s1y) < TAB_DELTA then s1x := 0;
 if abs(s2x) < TAB_DELTA then s1x := 0;
 if abs(s2y) < TAB_DELTA then s1x := 0;

 line.flaptype := FLAPTYPE_QUAD;
 if (sign(s1x) <> sign(s2x)) or (sign(s1y) <> sign(s2y)) then
 begin
     line.flaptype := FLAPTYPE_TRI;
     CalcTriFlapVertex(line, vy2, vx2, vy1, vx1, vertex1);
 end;

 flap.ftype := line.flaptype;
 flap.vertex_count := 3;
 if flap.ftype = FLAPTYPE_QUAD then
     flap.vertex_count := 4;
 flap.vertices[0].x := vertex1.x;
 flap.vertices[0].y := vertex1.y;
 flap.vertices[1].x := vertex2.x;
 flap.vertices[1].y := vertex2.y;
 flap.vertices[2].x := line.flapvert1x;
 flap.vertices[2].y := line.flapvert1y;
 flap.vertices[3].x := line.flapvert2x;
 flap.vertices[3].y := line.flapvert2y;
 result := flap;
end;


function InitLine(const x1, y1, x2, y2: single): TLine;
begin
  result.x1 := x1;
  result.y1 := y1;
  result.x2 := x2;
  result.y2 := y2;
end;

function GetIntersection(const a, b: TLine): TIntersectPoint;
var
  denom: single;
  x, y: single;
begin
  denom := (a.x1 - a.x2) * (b.y1 - b.y2)
         - (a.y1 - a.y2) * (b.x1 - b.x2);
  if denom = 0 then begin
      result.has_value := false;
      exit;
  end;
  x := (a.x1 * a.y2 - a.y1 * a.x2) * (b.x1 - b.x2)
     - (b.x1 * b.y2 - b.y1 * b.x2) * (a.x1 - a.x2);
  y := (a.x1 * a.y2 - a.y1 * a.x2) * (b.y1 - b.y2)
     - (b.x1 * b.y2 - b.y1 * b.x2) * (a.y1 - a.y2);
  result.x := x / denom;
  result.y := y / denom;
  result.has_value := true;
end;

{ ClipTab
  Test if line segments of a tab and edge list intersect. If yes, then set the tab vertex coords
  to the point of intersection, so segments don't cross. Also apply a slight downsizing (20%)
  to the tab, so there's a clear difference between the tab and the part's outline.
}
function ClipTab(tab: TFlap; const edges: TEdgeList): TFlap;

  procedure TestIntersect(const idx1, idx2: byte);
  var
    i: Integer;
    p: TIntersectPoint;
    a, b: TLine;
    clipped: boolean;
    minx, miny, maxx, maxy: single;
  begin
    a := InitLine(tab.vertices[idx1].x, tab.vertices[idx1].y,
                  tab.vertices[idx2].x, tab.vertices[idx2].y);
    clipped := false;
    minx := min(a.x1, a.x2);
    miny := min(a.y1, a.y2);
    maxx := max(a.x1, a.x2);
    maxy := max(a.y1, a.y2);
    for i := 0 to edges.Count - 1 do begin
        b := InitLine(edges[i][0].x, edges[i][0].y,
                      edges[i][1].x, edges[i][1].y);

        //if bounding rects don't intersect, don't test
        if (max(b.x1, b.x2) <= minx) or (maxx <= min(b.x1, b.x2)) or
           (max(b.y1, b.y2) <= miny) or (maxy <= min(b.y1, b.y2)) then
            continue;

        //line segments with same point intersect just at that point
        //(or have infinite intersecting points) - not interesting for our case
        if ((b.x1 = a.x1) or (b.x2 = a.x1)) and ((b.y1 = a.y1) or (b.y2 = a.y1)) then
            continue;

        //if the intersecting point is in range of our line, use it
        p := GetIntersection(a, b);
        if p.has_value then
            if (minx <= p.x) and (p.x <= maxx) and (miny <= p.y) and (p.y <= maxy)
            then begin
                a.x2 := p.x;
                a.y2 := p.y;
                minx := min(a.x1, a.x2);
                miny := min(a.y1, a.y2);
                maxx := max(a.x1, a.x2);
                maxy := max(a.y1, a.y2);
                clipped := true;
            end;
    end;
    if clipped then begin
        if (a.x1 <> a.x2) or (a.y1 <> a.y2) then begin
            a.x2 := a.x2 - (a.x2 - a.x1) * 0.2;
            a.y2 := a.y2 - (a.y2 - a.y1) * 0.2;
        end;
        tab.vertices[idx2].x := a.x2;
        tab.vertices[idx2].y := a.y2;
    end;
  end;

begin
  if tab.vertex_count = 3 then begin
      //check both sides of triangle
      TestIntersect(0, 2);
      TestIntersect(1, 2);
  end else begin
      //treat quad as two triangles. Fixes cases when the edge completely falls into part's area -
      //there are no edge intersections. Then check the mid line - it can be intersected by "spikes"
      TestIntersect(0, 3);
      TestIntersect(1, 3);

      TestIntersect(1, 2);
      TestIntersect(0, 2);

      TestIntersect(3, 2);
  end;
  result := tab;
end;

{ ClipTabs
  Clip each tab against a list of edges by moving one or both of its (outer) vertices
}
procedure ClipTabs(tabs: TFlapList; const edges: TEdgeList);
var
  i: integer;
begin
  for i := 0 to tabs.Count - 1 do begin
      tabs[i] := ClipTab(tabs[i], edges);
  end;
end;

end.
