var depth_first_0 = `

digraph {
    
    node [rx=5 ry=5 labelStyle="font: 300 14px 'Helvetica Neue', Helvetica"];

    edge [labelStyle="font: 300 14px 'Helvetica Neue', Helvetica"];

    she_sees_the_cat [labelType="html"
       label="<div class='avm hide'>
                 <table>
                    <tr>
                      <th>sem</th><td> ‍👁 ( _ , _ )</td>
                    </tr>
                  </table></div>"]; 

    she [labelType="html"
       label="<div class='avm hide'>
                 <table>
                    <tr>
                      <th>sem</th><td>‍👱‍♀️</td>
                    </tr>
                    <tr>
                      <th>str</th><td><i>she</i></td>
                    </tr>
                  </table></div>"]; 

    sees_the_cat [labelType="html"
       label="<div class='avm hide'>
                 <table>
                    <tr>
                      <th>sem</th><td> ‍👁 ( _ , _ )</td>
                    </tr>
                  </table></div>"]; 

    she_sees_the_cat -> she          [label="arg" labelStyle="fill: #55f; font-weight: bold;"];
    she_sees_the_cat -> sees_the_cat [label="f()" labelStyle="fill: #55f; font-weight: bold;"];
}

`;

var depth_first_05 = `

digraph {
    
    node [rx=5 ry=5 labelStyle="font: 300 14px 'Helvetica Neue', Helvetica"];

    edge [labelStyle="font: 300 14px 'Helvetica Neue', Helvetica"];

    she_sees_the_cat [labelType="html"
       label="<div class='avm hide'>
                 <table>
                    <tr>
                      <th>sem</th><td> ‍👁 ( _ , _ )</td>
                    </tr>
                  </table></div>"]; 

    she [labelType="html"
       label="<div class='avm hide'>
                 <table>
                    <tr>
                      <th>sem</th><td>‍👱‍♀️</td>
                    </tr>
                    <tr>
                      <th>str</th><td><i>she</i></td>
                    </tr>
                  </table></div>"]; 

    sees_the_cat [labelType="html"
       label="<div class='avm hide'>
                 <table>
                    <tr>
                      <th>sem</th><td> ‍👁 ( _ , _ )</td>
                    </tr>
                  </table></div>"]; 

    sees [labelType="html"
       label="<div class='avm hide'>
                 <table>
                    <tr>
                      <th>sem</th><td> ‍👁 ( _ , _ )</td>
                    </tr>
                    <tr>
                      <th>str</th><td><i>sees</i></td>
                    </tr>
                  </table></div>"]; 

    the_cat [labelType="html"
       label="<div class='avm hide'>
                 <table>
                    <tr>
                      <th>sem</th><td>‍🐱</td>
                    </tr>
                    <tr>
                      <th>str</th><td><i>the cat</i></td>
                    </tr>
                  </table></div>"]; 

    she_sees_the_cat -> she          [label="arg" labelStyle="fill: #55f; font-weight: bold;"];
    she_sees_the_cat -> sees_the_cat [label="f()" labelStyle="fill: #55f; font-weight: bold;"];
    sees_the_cat     -> sees         [label="f()" labelStyle="fill: #55f; font-weight: bold;"];
    sees_the_cat     -> the_cat      [label="arg" labelStyle="fill: #55f; font-weight: bold;"];
}

`;

var depth_first_1 = `

digraph {
    
    node [rx=5 ry=5 labelStyle="font: 300 14px 'Helvetica Neue', Helvetica"];

    edge [labelStyle="font: 300 14px 'Helvetica Neue', Helvetica"];

    she_sees_the_cat [labelType="html"
       label="<div class='avm hide'>
                 <table>
                    <tr>
                      <th>sem</th><td> ‍👁 ( _ , _ )</td>
                    </tr>
                  </table></div>"]; 

    she [labelType="html"
       label="<div class='avm hide'>
                 <table>
                    <tr>
                      <th>sem</th><td>‍👱‍♀️</td>
                    </tr>
                    <tr>
                      <th>str</th><td><i>she</i></td>
                    </tr>
                  </table></div>"]; 

    sees_the_cat [labelType="html"
       label="<div class='avm hide'>
                 <table>
                    <tr>
                      <th>sem</th><td> ‍👁 ( _ , _ )</td>
                    </tr>
                  </table></div>"]; 

    sees [labelType="html"
       label="<div class='avm'>
                 <table>
                    <tr>
                      <th>sem</th><td> ‍👁 ( _ , _ )</td>
                    </tr>
                    <tr>
                      <th>str</th><td><i>sees</i></td>
                    </tr>
                  </table></div>"]; 

    the_cat [labelType="html"
       label="<div class='avm hide'>
                 <table>
                    <tr>
                      <th>sem</th><td>‍🐱</td>
                    </tr>
                    <tr>
                      <th>str</th><td><i>the cat</i></td>
                    </tr>
                  </table></div>"]; 

    she_sees_the_cat -> she          [label="arg" labelStyle="fill: #55f; font-weight: bold;"];
    she_sees_the_cat -> sees_the_cat [label="f()" labelStyle="fill: #55f; font-weight: bold;"];
    sees_the_cat     -> sees         [label="f()" labelStyle="fill: #55f; font-weight: bold;"];
    sees_the_cat     -> the_cat      [label="arg" labelStyle="fill: #55f; font-weight: bold;"];
}

`;

var she_sees_the_cat = `

digraph {
    
    node [rx=5 ry=5 labelStyle="font: 300 14px 'Helvetica Neue', Helvetica"];

    edge [labelStyle="font: 300 14px 'Helvetica Neue', Helvetica"];

    she_sees_the_cat [labelType="html"
       label="<div class='avm'>
                 <table>
                    <tr>
                      <th>sem</th><td> 👁 (👱‍♀️,🐱)</td>
                    </tr>
                    <tr>
                      <th>str</th><td><i>she sees the cat</i></td>
                    </tr>
                  </table></div>"]; 

    she [labelType="html"
       label="<div class='avm'>
                 <table>
                    <tr>
                      <th>sem</th><td>‍👱‍♀️</td>
                    </tr>
                    <tr>
                      <th>str</th><td><i>she</i></td>
                    </tr>
                  </table></div>"]; 

    sees_the_cat [labelType="html"
       label="<div class='avm'>
                 <table>
                    <tr>
                      <th>sem</th><td> ‍👁 ( _ ,🐱)</td>
                    </tr>
                    <tr>
                      <th>str</th><td><i>sees the cat</i></td>
                    </tr>
                  </table></div>"]; 

    sees [labelType="html"
       label="<div class='avm'>
                 <table>
                    <tr>
                      <th>sem</th><td> ‍👁 ( _ , _ )</td>
                    </tr>
                    <tr>
                      <th>str</th><td><i>sees</i></td>
                    </tr>
                  </table></div>"]; 

    the_cat [labelType="html"
       label="<div class='avm'>
                 <table>
                    <tr>
                      <th>sem</th><td>‍🐱</td>
                    </tr>
                    <tr>
                      <th>str</th><td><i>the cat</i></td>
                    </tr>
                  </table></div>"]; 

    the [labelType="html"
       label="<div class='avm'>
                 <table>
                    <tr>
                      <th>sem</th><td></td>
                    </tr>
                    <tr>
                      <th>str</th><td><i>the</i></td>
                    </tr>
                  </table></div>"]; 

    cat [labelType="html"
       label="<div class='avm'>
                 <table>
                    <tr>
                      <th>sem</th><td>🐱</td>
                    </tr>
                    <tr>
                      <th>str</th><td><i>cat</i></td>
                    </tr>
                  </table></div>"]; 
    
    she_sees_the_cat  -> she;
    she_sees_the_cat  -> sees_the_cat;
    sees_the_cat      -> sees;
    sees_the_cat      -> the_cat;
    the_cat           -> the;
    the_cat           -> cat;
}

`;

var depth_first_2 = `

digraph {
    
    node [rx=5 ry=5 labelStyle="font: 300 14px 'Helvetica Neue', Helvetica"];

    edge [labelStyle="font: 300 14px 'Helvetica Neue', Helvetica"];

    she_sees_the_cat [labelType="html"
       label="<div class='avm hide'>
                 <table>
                    <tr>
                      <th>sem</th><td> ‍👁 ( _ , _ )</td>
                    </tr>
                  </table></div>"]; 

    she [labelType="html"
       label="<div class='avm hide'>
                 <table>
                    <tr>
                      <th>sem</th><td>‍👱‍♀️</td>
                    </tr>
                    <tr>
                      <th>str</th><td><i>she</i></td>
                    </tr>
                  </table></div>"]; 

    sees_the_cat [labelType="html"
       label="<div class='avm hide'>
                 <table>
                    <tr>
                      <th>sem</th><td> ‍👁 ( _ , _ )</td>
                    </tr>
                  </table></div>"]; 

    sees [labelType="html"
       label="<div class='avm'>
                 <table>
                    <tr>
                      <th>sem</th><td> ‍👁 ( _ , _ )</td>
                    </tr>
                    <tr>
                      <th>str</th><td><i>sees</i></td>
                    </tr>
                  </table></div>"]; 

    the_cat [labelType="html"
       label="<div class='avm hide'>
                 <table>
                    <tr>
                      <th>sem</th><td>‍🐱</td>
                    </tr>
                    <tr>
                      <th>str</th><td><i>the cat</i></td>
                    </tr>
                  </table></div>"]; 

    the [labelType="html"
       label="<div class='avm'>
                 <table>
                    <tr>
                      <th>sem</th><td></td>
                    </tr>
                    <tr>
                      <th>str</th><td><i>the</i></td>
                    </tr>
                  </table></div>"]; 

    cat [labelType="html"
       label="<div class='avm'>
                 <table>
                    <tr>
                      <th>sem</th><td>🐱</td>
                    </tr>
                    <tr>
                      <th>str</th><td><i>cat</i></td>
                    </tr>
                  </table></div>"]; 
    
    she_sees_the_cat -> she          [label="arg" labelStyle="fill: #55f; font-weight: bold; font-size:300%"];
    she_sees_the_cat -> sees_the_cat [label="f()" labelStyle="fill: #55f; font-weight: bold; font-size:300%"];
    sees_the_cat     -> sees         [label="f()" labelStyle="fill: #55f; font-weight: bold; font-size:300%"];
    sees_the_cat     -> the_cat      [label="arg" labelStyle="fill: #55f; font-weight: bold; font-size:300%"];

    the [labelType="html"
       label="<div class='avm hide'>
                 <table>
                    <tr>
                      <th>sem</th><td></td>
                    </tr>
                    <tr>
                      <th>str</th><td><i>the</i></td>
                    </tr>
                  </table></div>"]; 

    cat [labelType="html"
       label="<div class='avm'>
                 <table>
                    <tr>
                      <th>sem</th><td>🐱</td>
                    </tr>
                    <tr>
                      <th>str</th><td><i>cat</i></td>
                    </tr>
                  </table></div>"]; 

    the_cat           -> the       [label="arg" labelStyle="fill: #55f; font-weight: bold; font-size:300%"];
    the_cat           -> cat       [label="f()" labelStyle="fill: #55f; font-weight: bold; font-size:300%"];

    
}

`;
