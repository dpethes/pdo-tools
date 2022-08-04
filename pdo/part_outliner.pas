unit part_outliner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, glist;

type
  TEdgeVertex = record
    x, y: single;
  end;
  TEdge = array[0..1] of TEdgeVertex;
  TEdgeList = specialize TList<TEdge>;
  TVertexList = specialize TList<TEdgeVertex>;
  //multiple paths, each specified by a vertex list
  TPathList = specialize TList<TVertexList>;

  TOutlinerFace = record
    vertices: array of TEdgeVertex;
  end;
  TOutlinerFaceList = specialize TList<TOutlinerFace>;


//Finds all edges that were referenced only once
function FindOutlineEdges(const faces: TOutlinerFaceList): TEdgeList;
function MergeEdgesToPaths(const edgesToMerge: TEdgeList): TPathList;
procedure FreePathList(list: TPathList);

{
function EdgesFromLines(lines: array of TPdoLine; const faces: array of TPdoFace; const pedges: array of TPdoEdge): TEdgeList;
function ChainEdges(edges: TEdgeList): TVertexList;
}

implementation

const
{ vertex compare tolerance
  testing samples: "paper wing-", "02 - Rival RTX", "christmas"
}
  VERTEX_COMPARE_DELTA = 0.04;

operator = (const a, b: TEdgeVertex): boolean;
begin
  result := (abs(a.x - b.x) < VERTEX_COMPARE_DELTA) and (abs(a.y - b.y) < VERTEX_COMPARE_DELTA);
end;

function EdgeVertexEquals (const a, b: TEdgeVertex; const delta: single): boolean;
begin
  result := (abs(a.x - b.x) <= delta) and (abs(a.y - b.y) <= delta);
end;

function EdgeEquals (const a, b: TEdge; const delta: single): boolean;
begin
  result := (EdgeVertexEquals(a[0], b[0], delta) and EdgeVertexEquals(a[1], b[1], delta))
         or (EdgeVertexEquals(a[0], b[1], delta) and EdgeVertexEquals(a[1], b[0], delta))
end;


//adds new unique edge to the list or removes edge from list if already exists
procedure AddEdgeIfNew(var edges: TEdgeList; const face: TOutlinerFace; const v1, v2: integer);
var
  edge: TEdge;
  i: integer;
begin
  edge[0] := face.vertices[v1];
  edge[1] := face.vertices[v2];
  for i := 0 to edges.Count - 1 do begin
      if EdgeEquals(edge, edges[i], 0) then begin
          edges.Remove(i);
          exit;
      end;
  end;
  edges.Add(edge);
end;

function BuildEdgeList(const faces: TOutlinerFaceList): TEdgeList;
var
  edges: TEdgeList;
  face_i: integer;
  face: TOutlinerFace;
  i: integer;
begin
  edges := TEdgeList.Create;
  for face_i := 0 to faces.Count - 1 do begin
      face := faces[face_i];
      for i := 0 to Length(face.vertices) - 2 do begin
          AddEdgeIfNew(edges, face, i, i + 1);
      end;
      i := Length(face.vertices) - 1;
      AddEdgeIfNew(edges, face, i, 0);
  end;
  Result := edges;
end;

function JoinCloseEdges(var edges: TEdgeList): TEdgeList;
var
  occurences: integer;
  k: integer;
  i: integer;
  edge: TEdge;
begin
  result := TEdgeList.Create;
  for i := 0 to edges.Count - 1 do begin
      edge := edges[i];
      occurences := 0;
      for k := 0 to edges.Count - 1 do begin
          if EdgeEquals(edges[k], edge, VERTEX_COMPARE_DELTA) then
              occurences += 1;
          if occurences > 1 then break;
      end;
      if occurences <= 1 then
          result.add(edge);
  end;
end;

function FindOutlineEdges(const faces: TOutlinerFaceList): TEdgeList;
var
  edges: TEdgeList;
begin
  edges := BuildEdgeList(faces);
  result := JoinCloseEdges(edges);
  edges.Free;
end;


function FindNextByVertex(const edges: TEdgeList; const vert: TEdgeVertex): integer;
var
  i: integer;
begin
  result := -1;
  for i := 0 to edges.Count - 1 do begin
      if (edges[i][0] = vert) or (edges[i][1] = vert) then begin
          result := i;
          exit;
      end;
  end;
end;

function MergeEdgesToPaths(const edgesToMerge: TEdgeList): TPathList;
var
  edges: TEdgeList;
  edge: TEdge;
  vertices: TVertexList;
  last_point: TEdgeVertex;
  edge_idx: Integer;
begin
  result := TPathList.Create;
  edges := edgesToMerge.Copy;
  while edges.Count > 0 do begin
      edge := edges[0];
      edges.Remove(0);

      vertices := TVertexList.Create;
      vertices.Add(edge[0]);
      last_point := edge[1];

      while edges.Count > 0 do begin
          edge_idx := FindNextByVertex(edges, last_point);
          vertices.Add(last_point);
          if edge_idx < 0 then
              break;

          edge := edges[edge_idx];
          edges.Remove(edge_idx);
          if last_point = edge[0] then
              last_point := edge[1]
          else
              last_point := edge[0];
      end;

      result.Add(vertices);
  end;
  edges.Free;
end;

procedure FreePathList(list: TPathList);
var
  i: integer;
begin
  for i := 0 to list.Count - 1 do
      list[i].Free;
end;

end.

