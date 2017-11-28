BeginPackage["TestUtilities`"];

TestCase::usage="";
pass::usage="";
fail::usage="";
total::usage="";

TestResult::usage="";

CheckTrue::usage="";
CheckNotTrue::usage="";
CheckEqual::usage="";
CheckNotEqual::usage="";
CheckSameQ::usage="";
CheckNotSameQ::usage="";

GetTestNames::usage="";
GetTestBody::usage="";

ClearTest::usage="";
ClearAllTests::usage="";

RunTest::usage="";
RunAllTests::usage="";

PrintTestSummary::usage="";

GetTestBody::notest = "Test with name `1` not found";
ClearTest::notest = GetTestBody::notest;
TestCase::testexists = "Test with name `1` already exists";

Begin["`Private`"];

testCases = {};

GetTestNames[] := First /@ testCases;

GetTestBody[name_String /; If[MemberQ[GetTestNames[], name], True,
                              Message[GetTestBody::notest, name]; False]] :=
    First[Select[testCases, (#[[1]] == name)&]][[2]];

ClearTest[name_String /; If[MemberQ[GetTestNames[], name], True,
                              Message[ClearTest::notest, name]; False]] :=
    testCases = Select[testCases, (#[[1]] != name)&];

ClearAllTests[] := testCases = {};

GetAssertionCount[result_, tag_] :=
    Length[Flatten[Cases[result, HoldPattern[Rule[tag, v_]] :> v]]];

RunTest[name_String /; If[MemberQ[GetTestNames[], name], True,
                          Message[RunTest::notest, name]; False]] :=
    Module[{totalAssertions = 0, passedAssertions = 0,
            failedAssertions = 0, testTime, output, results},
           {testTime, {output, results}} = AbsoluteTiming[Reap[ReleaseHold[GetTestBody[name]], _, Rule]];
           passedAssertions = GetAssertionCount[results, TestUtilities`pass];
           failedAssertions = GetAssertionCount[results, TestUtilities`fail];
           totalAssertions = GetAssertionCount[results, TestUtilities`total];
           TestResult[name, totalAssertions, passedAssertions, failedAssertions, testTime]
          ];

RunAllTests[] := RunTest /@ GetTestNames[];

SetAttributes[TestCase, HoldRest];

TestCase[name_String /; If[FreeQ[GetTestNames[], name], True,
                           Message[TestCase::testexists, name]; False],
         body_] :=
   AppendTo[testCases, {name, HoldComplete[body]}];

TestPassed[result_] := Sow[result, {TestUtilities`pass, TestUtilities`total}];
TestFailed[result_] := Sow[result, {TestUtilities`fail, TestUtilities`total}];

SetAttributes[CheckTrue, HoldAll];
SetAttributes[CheckNotTrue, HoldAll];
SetAttributes[CheckEqual, HoldAll];
SetAttributes[CheckNotEqual, HoldAll];
SetAttributes[CheckSameQ, HoldAll];
SetAttributes[CheckNotSameQ, HoldAll];

CheckTrue[expr_] :=
    Module[{logMsg},
           logMsg = "TrueQ[" <> ToString[InputForm[expr]] <> "]";
           If[TrueQ[ReleaseHold[expr]],
              TestPassed[logMsg],
              TestFailed[logMsg]
             ];
          ];

CheckNotTrue[expr_] :=
    Module[{logMsg},
           logMsg = "!TrueQ[" <> ToString[InputForm[expr]] <> "]";
           If[TrueQ[ReleaseHold[expr]],
              TestFailed[logMsg],
              TestPassed[logMsg]
             ];
          ];

CheckEqual[lhs_, rhs_] :=
    Module[{logMsg},
           logMsg = ToString[InputForm[lhs]] <> " == "
                    <> ToString[InputForm[rhs]];
           If[ReleaseHold[lhs == rhs],
              TestPassed[logMsg],
              TestFailed[logMsg];
             ];
          ];

CheckNotEqual[lhs_, rhs_] :=
    Module[{logMsg},
           logMsg = ToString[InputForm[lhs]] <> " != "
                    <> ToString[InputForm[rhs]];
           If[ReleaseHold[lhs != rhs],
              TestPassed[logMsg],
              TestFailed[logMsg]
             ];
          ];

CheckSameQ[lhs_, rhs_] :=
    Module[{logMsg},
           logMsg = ToString[InputForm[lhs]] <> " === "
                    <> ToString[InputForm[rhs]];
           If[ReleaseHold[SameQ[lhs, rhs]],
              TestPassed[logMsg],
              TestFailed[logMsg]
             ];
          ];

CheckNotSameQ[lhs_, rhs_] :=
    Module[{logMsg},
           logMsg = ToString[InputForm[lhs]] <> " =!= "
                    <> ToString[InputForm[rhs]];
           If[ReleaseHold[!SameQ[lhs, rhs]],
              TestPassed[logMsg],
              TestFailed[logMsg]
             ];
          ];

PrintTestSummary[TestResult[name_String, total_Integer,
                            passed_Integer, failed_Integer, timing_]] :=
    Block[{},
          Print["-------------"];
          Print["Test case: ", name];
          Print["-------------"];
          Print["STATUS: ",
                If[failed != 0, Style["FAIL", Red], Style["OK", Green]]];
          Print["Checked ", total, " assertions in ", timing, " seconds."];
         ];

End[];

EndPackage[];
