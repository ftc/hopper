package edu.colorado.plv.unittest;

import android.app.Activity;
import android.view.View;
import android.os.Bundle;
import android.widget.*;
import android.util.Log;

public class MainActivity extends Activity {
	RelativeLayout layout;
	
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        layout = (RelativeLayout)RelativeLayout.inflate(this, R.layout.activity_main, null);
        setContentView(layout);
        layout.toString();
	}
	protected void dynamicCallMethod(){
        Log.i("some","log message");
    }
}
